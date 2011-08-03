#include <erl_driver.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/uio.h>

#include <sys/ioctl.h>
#include <sys/mman.h>
#include <linux/videodev2.h>
#include <libswscale/swscale.h>

#define V4L_BUFFERS_DEFAULT	8

enum {
  CMD_OPEN = 1
};

struct buffer
{
	unsigned int size;
	void *mem;
};

#pragma pack(1)
typedef struct {
  uint8_t device;
  uint16_t width;
  uint16_t height;
  uint8_t fps;
} Config;
#pragma options align=reset

typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner_pid;
  int fd;
  struct buffer *buffers;
  int nbufs;
  unsigned int pixelformat;
  int width;
  int height;
  struct SwsContext* scale_ctx;
} Uvc;




static ErlDrvData uvc4erl_drv_start(ErlDrvPort port, char *buff)
{
    Uvc* d = (Uvc *)driver_alloc(sizeof(Uvc));
    bzero(d, sizeof(Uvc));
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->owner_pid = driver_caller(port);
    d->fd = -1;
    return (ErlDrvData)d;
}


static void uvc4erl_drv_stop(ErlDrvData handle)
{
  Uvc* d = (Uvc *)handle;
  driver_select(d->port, (ErlDrvEvent)(d->fd), DO_READ|DO_WRITE, 0);
  close(d->fd);
  driver_free((char*)handle);
}


static void uvc4erl_exit(Uvc *d)
{
  driver_select(d->port, (ErlDrvEvent)d->fd, DO_READ|DO_WRITE, 0);
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("uvc4erl_closed"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_TUPLE, 2
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  driver_exit(d->port, 0);
}

static int video_alloc_buffers(Uvc *dev);

static int uvc4erl_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen) {
  Uvc* d = (Uvc*) handle;
  
  switch(command) {
    case CMD_OPEN: {
      
      fprintf(stderr, "Opening\r\n");
      if(len != sizeof(Config)) {
        driver_failure_atom(d->port, "invalid_config");
        return 0;
      }
      Config *cfg = (Config *)buf;
      char device_path[1024];
      snprintf(device_path, sizeof(device_path), "/dev/video%d", cfg->device);
      d->fd = open(device_path, O_RDWR);
      if(d->fd == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
      }

      fprintf(stderr, "Opened device %s\r\n", device_path);

    	struct v4l2_capability cap;
    	memset(&cap, 0, sizeof cap);
      int ret;
    	ret = ioctl(d->fd, VIDIOC_QUERYCAP, &cap);
    	if (ret < 0) {
        driver_failure_posix(d->port, errno);
    		return 0;
    	}
       
      if(!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)) {
        driver_failure_atom(d->port, "not_a_capture_device");
        return 0;
      }

      fprintf(stderr, "Setting size %dx%d\r\n", cfg->width, cfg->height);
      
      struct v4l2_format fmt;
    	memset(&fmt, 0, sizeof fmt);
    	fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    	if(cfg->width > 640 || cfg->height > 480) {
        d->pixelformat = V4L2_PIX_FMT_MJPEG;
    	} else {
        d->pixelformat = V4L2_PIX_FMT_YUYV;
    	}
      d->width = cfg->width;
      d->height = cfg->height;
    	fmt.fmt.pix.width = cfg->width;
    	fmt.fmt.pix.height = cfg->height;
    	fmt.fmt.pix.pixelformat = d->pixelformat;
    	fmt.fmt.pix.field = V4L2_FIELD_ANY;
    	
    	ret = ioctl(d->fd, VIDIOC_S_FMT, &fmt);
    	if (ret < 0) {
        driver_failure_posix(d->port, errno);
    		return 0;
    	}
    	
      fprintf(stderr, "Setting fps %d\r\n", cfg->fps);
    	
    	struct v4l2_streamparm parm;

    	memset(&parm, 0, sizeof parm);
      parm.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

    	ret = ioctl(d->fd, VIDIOC_G_PARM, &parm);
    	if (ret < 0) {
        driver_failure_posix(d->port, errno);
        return 0;
    	}
    	parm.parm.capture.timeperframe.numerator = 1;
    	parm.parm.capture.timeperframe.denominator = cfg->fps;
    	
    	ret = ioctl(d->fd, VIDIOC_S_PARM, &parm);
    	if (ret < 0) {
        driver_failure_posix(d->port, errno);
        return 0;
    	}
    	
      fprintf(stderr, "Setting quality 100\r\n");
      struct v4l2_jpegcompression jpeg;
      memset(&jpeg, 0, sizeof jpeg);
      jpeg.quality = 90;
      
      ret = ioctl(d->fd, VIDIOC_S_JPEGCOMP, &jpeg);
      if (ret < 0) {
        // driver_failure_posix(d->port, errno);
        // return 0;
        fprintf(stderr, "Failed to set quality\r\n");
      }
    	
    	
      fprintf(stderr, "Capture ready, prepare start \r\n");
    	
    	if(video_alloc_buffers(d) < 0) {
        driver_failure_posix(d->port, errno);
        return 0;
    	}
    	
      fprintf(stderr, "Allocated buffers\r\n");

      int type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    	ret = ioctl(d->fd, VIDIOC_STREAMON, &type);
    	if(ret < 0) {
        driver_failure_posix(d->port, errno);
        return 0;
    	}
      
      fprintf(stderr, "Capture started\r\n");

      driver_select(d->port, (ErlDrvEvent)d->fd, DO_READ, 1);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    break;
    
    
    default:
    return 0;
  }
  return 0;
}


static int video_queue_buffer(Uvc *dev, int index)
{
	struct v4l2_buffer buf;
	int ret;

	memset(&buf, 0, sizeof buf);
	buf.index = index;
	buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	buf.memory = V4L2_MEMORY_MMAP;
	buf.length = dev->buffers[index].size;

	memset(dev->buffers[buf.index].mem, 0x55, buf.length);

	ret = ioctl(dev->fd, VIDIOC_QBUF, &buf);
	if (ret < 0)
		printf("Unable to queue buffer (%d).\n", errno);

	return ret;
}


static int video_alloc_buffers(Uvc *dev)
{
	struct v4l2_requestbuffers rb;
	struct v4l2_buffer buf;
	struct buffer *buffers;
	unsigned int i;
	int ret;
  int nbufs = V4L_BUFFERS_DEFAULT;

	memset(&rb, 0, sizeof(rb));
	rb.count = nbufs;
	rb.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	rb.memory = V4L2_MEMORY_MMAP;

	ret = ioctl(dev->fd, VIDIOC_REQBUFS, &rb);
	if (ret < 0) {
    fprintf(stderr, "Failed request buffer\r\n");
		return ret;
	}

	buffers = malloc(rb.count * sizeof buffers[0]);
	if (buffers == NULL)
		return -ENOMEM;

	/* Map the buffers. */
	for (i = 0; i < rb.count; ++i) {
		memset(&buf, 0, sizeof buf);
		buf.index = i;
		buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		buf.memory = V4L2_MEMORY_MMAP;
		ret = ioctl(dev->fd, VIDIOC_QUERYBUF, &buf);
		if (ret < 0) {
			fprintf(stderr, "Unable to query buffer %u (%d).\r\n", i, errno);
			return ret;
		}
		printf("length: %u offset: %u\n", buf.length, buf.m.offset);

		buffers[i].mem = mmap(0, buf.length, PROT_READ | PROT_WRITE, MAP_SHARED, dev->fd, buf.m.offset);
		if (buffers[i].mem == MAP_FAILED) {
			fprintf(stderr, "Unable to map buffer %u (%d)\r\n", i, errno);
			return -1;
		}
		buffers[i].size = buf.length;
		printf("Buffer %u mapped at address %p.\n", i, buffers[i].mem);
	}
	dev->buffers = buffers;
	dev->nbufs = rb.count;
	
  fprintf(stderr, "Queing buffers\r\n");
	
  for(i = 0; i < rb.count; i++) {
    ret = video_queue_buffer(dev, i);
    if(ret < 0) return ret;
  }

	return 0;
}

uint8_t huffman_table[] = "\xFF\xC4\x01\xA2\x00\x00\x01\x05\x01\x01\x01\x01"\
      "\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02"\
      "\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x01\x00\x03"\
      "\x01\x01\x01\x01\x01\x01\x01\x01\x01\x00\x00\x00"\
      "\x00\x00\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09"\
      "\x0A\x0B\x10\x00\x02\x01\x03\x03\x02\x04\x03\x05"\
      "\x05\x04\x04\x00\x00\x01\x7D\x01\x02\x03\x00\x04"\
      "\x11\x05\x12\x21\x31\x41\x06\x13\x51\x61\x07\x22"\
      "\x71\x14\x32\x81\x91\xA1\x08\x23\x42\xB1\xC1\x15"\
      "\x52\xD1\xF0\x24\x33\x62\x72\x82\x09\x0A\x16\x17"\
      "\x18\x19\x1A\x25\x26\x27\x28\x29\x2A\x34\x35\x36"\
      "\x37\x38\x39\x3A\x43\x44\x45\x46\x47\x48\x49\x4A"\
      "\x53\x54\x55\x56\x57\x58\x59\x5A\x63\x64\x65\x66"\
      "\x67\x68\x69\x6A\x73\x74\x75\x76\x77\x78\x79\x7A"\
      "\x83\x84\x85\x86\x87\x88\x89\x8A\x92\x93\x94\x95"\
      "\x96\x97\x98\x99\x9A\xA2\xA3\xA4\xA5\xA6\xA7\xA8"\
      "\xA9\xAA\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xC2"\
      "\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xD2\xD3\xD4\xD5"\
      "\xD6\xD7\xD8\xD9\xDA\xE1\xE2\xE3\xE4\xE5\xE6\xE7"\
      "\xE8\xE9\xEA\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9"\
      "\xFA\x11\x00\x02\x01\x02\x04\x04\x03\x04\x07\x05"\
      "\x04\x04\x00\x01\x02\x77\x00\x01\x02\x03\x11\x04"\
      "\x05\x21\x31\x06\x12\x41\x51\x07\x61\x71\x13\x22"\
      "\x32\x81\x08\x14\x42\x91\xA1\xB1\xC1\x09\x23\x33"\
      "\x52\xF0\x15\x62\x72\xD1\x0A\x16\x24\x34\xE1\x25"\
      "\xF1\x17\x18\x19\x1A\x26\x27\x28\x29\x2A\x35\x36"\
      "\x37\x38\x39\x3A\x43\x44\x45\x46\x47\x48\x49\x4A"\
      "\x53\x54\x55\x56\x57\x58\x59\x5A\x63\x64\x65\x66"\
      "\x67\x68\x69\x6A\x73\x74\x75\x76\x77\x78\x79\x7A"\
      "\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x92\x93\x94"\
      "\x95\x96\x97\x98\x99\x9A\xA2\xA3\xA4\xA5\xA6\xA7"\
      "\xA8\xA9\xAA\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA"\
      "\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xD2\xD3\xD4"\
      "\xD5\xD6\xD7\xD8\xD9\xDA\xE2\xE3\xE4\xE5\xE6\xE7"\
      "\xE8\xE9\xEA\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA";



size_t add_huffman(uint8_t *dst, uint8_t *src, size_t size) {
  size_t len = 0;
  if(src[0] != 0xFF || src[1] != 0xD8) return len;
  memcpy(dst, src, 2);
  dst += 2;
  src += 2;
  len += 2;
  size -= 2;
  
  int has_dht = 0;
  
  while(size > 0) {
    if(src[0] != 0xFF) return 0;
    if(src[1] == 0xC4) has_dht = 1;
    if(src[1] == 0xDA) break;
    
    size_t blocksize = ((src[2] << 8) | src[3]) + 2;
    memcpy(dst, src, blocksize);
    dst += blocksize;
    src += blocksize;
    len += blocksize;
    size -= blocksize;
  }
  if(!has_dht) {
    memcpy(dst, huffman_table, sizeof(huffman_table));
    dst += sizeof(huffman_table);
    len += sizeof(huffman_table);
  }
  memcpy(dst, src, size);
  dst += size;
  src += size;
  len += size;
  
  return len;
}

static void uvc4erl_drv_input(ErlDrvData handle, ErlDrvEvent io_event)
{
  Uvc* d = (Uvc*) handle;
  struct v4l2_buffer buf;
	
  memset(&buf, 0, sizeof buf);
	buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	buf.memory = V4L2_MEMORY_MMAP;
	int ret = ioctl(d->fd, VIDIOC_DQBUF, &buf);
	if(ret < 0) {
    driver_failure_posix(d->port, errno);
    return;
	}
	
  ErlDrvBinary* bin;
  size_t len;
  if(d->pixelformat == V4L2_PIX_FMT_YUYV) {
    len = d->width*d->height*3/2;
    bin = driver_alloc_binary(len);
    
    if(!d->scale_ctx) d->scale_ctx = sws_getContext(
      d->width, d->height, PIX_FMT_YUYV422, 
      d->width, d->height, PIX_FMT_YUV420P, 
      SWS_FAST_BILINEAR, NULL, NULL, NULL
    );

    int linesize[4] = {d->width*2, 0, 0, 0};
    uint8_t *src[4] = {d->buffers[buf.index].mem, 0, 0, 0};

    int stride_size = d->width*d->height;
    uint8_t *plane[4] = {bin->orig_bytes, bin->orig_bytes+stride_size, bin->orig_bytes+stride_size+stride_size/4, NULL};
    int stride[4] = {d->width, d->width/2, d->width/2, 0};
    
    sws_scale(d->scale_ctx, src, linesize, 0, d->height, plane, stride);
  } else {
    bin = driver_alloc_binary(buf.bytesused + 1024);
    len = add_huffman((uint8_t *)bin->orig_bytes, (uint8_t *)d->buffers[buf.index].mem, buf.bytesused);
  }
  
  
  ErlDrvUInt64 pts = buf.timestamp.tv_sec * 1000 + buf.timestamp.tv_usec / 1000;
  
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("uvc4erl"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_ATOM, driver_mk_atom(d->pixelformat == V4L2_PIX_FMT_YUYV ? "yuv" : "jpeg"),
    ERL_DRV_UINT64, &pts,
    ERL_DRV_BINARY, (ErlDrvTermData)bin, (ErlDrvTermData)len, 0,
    ERL_DRV_TUPLE, 5
  };


  // fprintf(stderr, "Event in uvc: %lu %u %u\r\n", len, (unsigned)buf.timestamp.tv_sec, (unsigned)buf.timestamp.tv_usec);
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  
  ret = video_queue_buffer(d, buf.index);
	if(ret < 0) {
    driver_failure_posix(d->port, errno);
    return;
	}
}


ErlDrvEntry uvc4erl_driver_entry = {
    NULL,			/* F_PTR init, N/A */
    uvc4erl_drv_start,		/* L_PTR start, called when port is opened */
    uvc4erl_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,	                /* F_PTR output, called when erlang has sent */
    uvc4erl_drv_input,		/* F_PTR ready_input, called when input descriptor ready */
    NULL,	              /* F_PTR ready_output, called when output descriptor ready */
    "uvc4erl_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    uvc4erl_drv_command,			/* F_PTR control, port_command callback */
    NULL,		    	/* F_PTR timeout, reserved */
    NULL,	                     /* F_PTR outputv, reserved */
    NULL,                      /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(uvc4erl_drv) /* must match name in driver_entry */
{
    return &uvc4erl_driver_entry;
}
