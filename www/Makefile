ROOTDIR = ./
DRAFTS  = $(ROOTDIR)/drafts
POSTS   = $(ROOTDIR)/posts
PAGES   = $(ROOTDIR)/pages

EXECUTABLE = bsima

site: bin/$(EXECUTABLE).hs
	ghc --make bin/$(EXECUTABLE).hs -optl -w

build:
	bin/$(EXECUTABLE) build

watch:
	bin/$(EXECUTABLE) watch

sync: _site
	aws s3 sync _site/ s3://www.bsima.me/

clean:
	bin/$(EXECUTABLE) clean
	find . -name '*~' | xargs rm -f
	find . -name '#*#' | xargs rm -f
	find . -name '*.o' | xargs rm -f
	rm bin/$(EXECUTABLE).hi

setup-nix:
	cabal2nix --shell ./ > shell.nix
	nix-shell -I ./ --command 'cabal configure'

draft:
	read -p "Slug: " slug
	read -p "Title: " title
	while true
	do
		echo "---\ntitle: $title\nmath: true\n---\n\n" > $(DRAFTS)/$(date +%Y-%m-%d)-$slug.markdown
	done
	echo "All done!\n"
