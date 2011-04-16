require 'nokogiri'

# Basic thematic mapping. Takes an SVG file which uses country ISO
# codes as IDs for countries, and fills in the backgrounds based on
# the colour scale and magnitudes given. Only linear colour scales are
# supported at present, not divergent or
module ThematicMap
  class Config
    @@map_file = :world_compact
    @@output_format = 'svg'
    @@output_filename = 'thematic_map'

    @@colour_palette = []

    # Create accessors for class variables (used by all subclasses, so
    # can be set anywhere.
    class_variables.each do |var|
      # Provide aliases for colour / color variation.
      methods = [var, var.gsub('colour', 'color')].uniq.map {|x| x[2..-1]}

      (class << self; self; end).class_eval do
        methods.each do |meth|
          define_method(meth) {class_variable_get(var)}
          define_method("#{meth}=") {|x| class_variable_set(var, x)}
        end
      end
    end
  end
end
