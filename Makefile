# guix-forge --- Guix software forge meta-service
# Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
#
# This file is part of guix-forge.
#
# guix-forge is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# guix-forge is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with guix-forge.  If not, see <https://www.gnu.org/licenses/>.

EMACS = emacs
GUILD = guild
SKRIBILO = skribilo

sources = $(wildcard forge/*.scm) $(wildcard forge/*/*.scm)
doc_skribilo_config = doc/skribilo.scm
doc_skribilo_config_go = $(doc_skribilo_config:.scm=.go)
doc_sources = doc/forge.skb
doc_html = $(doc_sources:.skb=.html)

.PHONY: all html clean
all: ;

%.go: %.scm
	$(GUILD) compile -L . -o $@ $<

html: $(doc_html)

$(doc_html): $(doc_sources) $(sources) $(doc_skribilo_config_go)
	rm -rf $@
	mkdir -p $@
	GUILE_LOAD_PATH=$(CURDIR):$(GUILE_LOAD_PATH) GUILE_LOAD_COMPILED_PATH=$(CURDIR):$(GUILE_LOAD_COMPILED_PATH) $(SKRIBILO) --target=html $< --output=$@/index.html

website: website/index.html website/manual/dev/en

website/index.html: README.org build-aux/build-home-page.el
	$(EMACS) -Q --batch --load build-aux/build-home-page.el --funcall build-website

website/manual/dev/en: $(doc_html)
	rm -rf $@
	mkdir -p $(dir $@)
	cp -vr $^ $@

clean:
	rm -f $(doc_skribilo_config_go)
