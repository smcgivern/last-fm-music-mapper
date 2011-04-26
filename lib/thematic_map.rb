require 'nokogiri'

# Basic thematic mapping. Takes an SVG file which uses country ISO
# codes as IDs for countries, and fills in the backgrounds based on
# the colour scale and magnitudes given. Only linear colour scales are
# supported at present, not divergent or
module ThematicMap
  class << self
    @@map_file = :world_compact
    @@output_format = 'svg'
    @@output_filename = 'thematic_map'

    # http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_RGB.html
    @@colour_palette = ['#000000']

    def create_map; self.colour_palette; end
  end

  # Create accessors for class variables (used by all subclasses, so
  # can be set anywhere.
  class_variables.each do |var|
    # Provide aliases for colour / color variation.
    methods = [var, var.gsub('colour', 'color')].uniq.map {|x| x[2..-1]}

    methods.each do |meth|
      (class << self; self; end).class_eval do
        self.send(:define_method, meth) {class_variable_get(var)}
        define_method("#{meth}=") {|x| class_variable_set(var, x)}
      end
    end
  end
end
