include version.mk

all: compile


compile:
	rm -f ._rebar.config
	rm -f src/._*
	rebar compile

package: compile
	rm -rf tmproot
	mkdir -p tmproot/opt/erlyvideo/lib/uvc-$(VERSION)
	cp -r src priv ebin tmproot/opt/erlyvideo/lib/uvc-$(VERSION)
	cd tmproot && \
	fpm -s dir -t deb -n erly-uvc -v $(VERSION) -m "Max Lapshin <max@maxidoors.ru>" opt
	mv tmproot/*.deb .

upload_package: 
	scp *$(VERSION)* erlyhub@git.erlyvideo.org:/apps/erlyvideo/debian/public/transcoding
	ssh erlyhub@git.erlyvideo.org "cd /apps/erlyvideo/debian ; ./update transcoding"
