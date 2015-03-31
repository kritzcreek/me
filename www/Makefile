ROOTDIR = ./
DRAFTS  = $(ROOTDIR)/drafts
POSTS   = $(ROOTDIR)/posts
PAGES   = $(ROOTDIR)/pages


#make_post(DIR) {
#	read -p "Slug: " slug
#	read -p "Title: " title
#	while true
#	do
#		echo "---\ntitle: $title\nmath: true\n---\n\n" > $(DIR)/$(date +%Y-%m-%d)-$slug.markdown
#	done
#	echo "\nAll done!\n"
#}


site: bin/site.hs
	ghc --make bin/site.hs -optl -w

build:
	bin/site build

watch:
	bin/site watch

sync: _site
	s3cmd -P sync _site/ s3://www.bsima.me/

clean:
	bin/site clean
	find . -name '*~' | xargs rm -f
	find . -name '#*#' | xargs rm -f
	find . -name '*.o' | xargs rm -f
	rm bin/site.hi

draft:
	read -p "Slug: " slug
	read -p "Title: " title
	while true
	do
		echo "---\ntitle: $title\nmath: true\n---\n\n" > $(DRAFTS)/$(date +%Y-%m-%d)-$slug.markdown
	done
	echo "All done!\n"
