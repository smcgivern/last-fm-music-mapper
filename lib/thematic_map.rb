require 'nokogiri'

# Basic thematic mapping. Takes an SVG file which uses country ISO
# codes as IDs for countries, and fills in the backgrounds based on
# the colour scale and magnitudes given. Only linear colour scales are
# supported at present, not divergent or
module ThematicMap
  class << self
    @@map = :world_compact
    @@output_format = 'svg'
    @@output_filename = 'thematic_map'

    # http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_RGB.html
    @@colour_palette = ['#000000']

    def map_file; Symbol === map ? "lib/thematic_map/maps/#{map}.svg" : map; end
    def map_xml; Nokogiri::XML(open(map_file)); end

    def create_map(iso_codes)
      output_xml = map_xml

      iso_codes.each do |iso_code|
        output_xml.search("##{iso_code} path").each do |path|
          path['style'] = "fill : #{colour_palette.first};"
        end
      end

      open("#{output_filename}.#{output_format}", 'w').puts(output_xml.to_xml)
    end
  end

  # Create accessors for class variables (used by all subclasses, so
  # can be set anywhere.
  class_variables.each do |var|
    # Provide aliases for colour / color variation.
    methods = [var, var.gsub('colour', 'color')].uniq.map {|x| x[2..-1]}

    methods.each do |meth|
      (class << self; self; end).class_eval do
        define_method(meth) {class_variable_get(var)}
        define_method("#{meth}=") {|x| class_variable_set(var, x)}
      end
    end
  end
end
