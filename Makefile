.PHONY: install

install: MAKE_STATION = $(PWD)/make_station/bin/make_station

install:
	rm -rf install /tmp/missions /tmp/make_mission /tmp/game
	cd make_station; alr build; cd -
	cp -v -r share/mimsy/missions /tmp/
	cp -v -r make_station/share/make_station /tmp/make_mission
	cd /tmp/missions; \
	 for J in `jq -r .missions[] < library.json`; do \
	   git clone --depth=1 https://github.com/CheckiO-Missions/$$J ;\
	 done ;\
	cd -
	cd /tmp; $(MAKE_STATION) missions/library.json; cd -
	alr exec -- gprbuild -p -P ../matreshka/design/xhtml2html5/xhtml2html5.gpr
	mkdir -p install/WEB-INF/lib/x86_64-linux
	mv -v /tmp/game install/
	ln -v -s $$PWD/.lib/libmimsy.so install/WEB-INF/lib/x86_64-linux/
	ln -v -s $$PWD/source/web.xml install/WEB-INF/
	../matreshka/design/xhtml2html5/.objs/xhtml2html5 share/mimsy/index.xhtml > install/game/index.html
	cp -v share/mimsy/*.ico install/
	cp -v share/mimsy/*.tmpl install/
	cp -v -r share/mimsy/js install/
	cp -v -r share/mimsy/graphics install/
	cp -v -r share/mimsy/svg install/game/
	cp -v share/mimsy/*.css install/game/
	cp -v share/mimsy/*.js install/game/
	curl -o install/game/pure-min.css https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css
	curl -L https://registry.npmjs.org/monaco-editor/-/monaco-editor-0.39.0.tgz |\
	  tar xzf - -Cinstall/game package/min --strip-components=1
