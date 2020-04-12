public/ext/style.css: template/css/style.scss
	@sass template/css/style.scss public/ext/style.css

preview: public/ext/style.css
	@stack run

-include *.mk
