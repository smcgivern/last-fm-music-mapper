public/ext/style.css: template/css/style.scss
	@sassc template/css/style.scss public/ext/style.css

preview: public/ext/style.css
	@stack run --nix --nix-packages zlib

-include *.mk
