include version.mk

all: compile


compile:
	rm -f ._rebar.config
	rebar compile

packages: compile
	rm -rf tmproot
	tar zcf uvc-$(VERSION).tgz uvc
	mkdir -p tmproot/opt/erlyvideo/lib/uvc-$(VERSION)
	cp -r priv ebin tmproot/opt/erlyvideo/lib/uvc-$(VERSION)
	cd tmproot && \
	fpm -s dir -t deb -n erlyvideo -v $(VERSION) -m "Max Lapshin <max@maxidoors.ru>" etc/init.d/erlyvideo opt
	mv tmproot/*.deb tmproot/*.rpm .

upload_packages: 
	scp *$(VERSION)* erlyhub@git.erlyvideo.org:/apps/erlyvideo/debian/public/binary
	ssh erlyhub@git.erlyvideo.org "cd /apps/erlyvideo/debian ; ./update"
#	echo "Erlyvideo version ${VERSION} uploaded to debian repo http://debian.erlyvideo.org/ ." | mail -r "Erlybuild <build@erlyvideo.org>" -s "Erlyvideo version ${VERSION}" -v erlyvideo-dev@googlegroups.com
