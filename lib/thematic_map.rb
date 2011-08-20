require 'nokogiri'

# Basic thematic mapping. Takes an SVG file which uses country ISO
# codes as IDs for countries, and fills in the backgrounds based on
# the colour scale and magnitudes given.
class ThematicMap
  attr_accessor :map, :division_method, :division_size, :colour_palette
  alias :color_palette :colour_palette

  def initialize
    # Map file to use; if a symbol, taken from lib/thematic_map/maps/ directory.
    @map = :world_compact

    # Method to use for dividing data into colour bands.
    @division_method = :equal_ranges_division

    # Average size of divisions.
    @division_size = 5

    # http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_RGB.html
    # Yellow-green.
    @colour_palette = [
                       ['#f7fcb9', '#addd8e', '#31a354'],
                       ['#ffffcc', '#c2e699', '#78c679', '#238443'],
                       ['#ffffcc', '#c2e699', '#78c679', '#31a354', '#006837'],
                       ['#ffffcc', '#d9f0a3', '#addd8e', '#78c679', '#31a354',
                        '#006837'],
                       ['#ffffcc', '#d9f0a3', '#addd8e', '#78c679', '#41ab5d',
                        '#238443', '#005a32'],
                       ['#ffffe5', '#f7fcb9', '#d9f0a3', '#addd8e', '#78c679',
                        '#41ab5d', '#238443', '#005a32'],
                       ['#ffffe5', '#f7fcb9', '#d9f0a3', '#addd8e', '#78c679',
                        '#41ab5d', '#238443', '#006837', '#004529']
                      ]

    yield self if block_given?
  end

  def map_file
    if Symbol === map
      "#{File.dirname(__FILE__)}/thematic_map/maps/#{map}.svg"
    else
      map
    end
  end

  def map_xml; Nokogiri::XML(open(map_file)); end

  # Calculates the number of divisions needed for the values, given
  # @@division_size. Will only pick values which match the length of
  # a sub-array of @@colour_palette.
  def number_of_divisions(values)
    ideal_number = values.uniq.length / division_size
    potential_numbers = colour_palette.map {|x| x.length}

    return ideal_number if potential_numbers.include?(ideal_number)
    return potential_numbers.max if ideal_number > potential_numbers.max
    return potential_numbers.min if ideal_number < potential_numbers.min
  end

  def value_division(value, values)
    send(division_method, values).each_with_index.
      select {|x, i| x <= value}.
      max_by {|y, j| y}.
      last
  end

  def equal_count_division(values)
    range_size = values.uniq.size / number_of_divisions(values)
    ranges = Array.new(number_of_divisions(values))

    ranges.each_with_index.map {|x, i| values.uniq[range_size * i]}
  end

  def equal_ranges_division(values)
    range_size = (values.max - values.min + 1) / number_of_divisions(values)
    ranges = Array.new(number_of_divisions(values))

    ranges.each_with_index.map {|x, i| values.min + range_size * i}
  end

  # Takes a hash where the keys are ISO codes and the values are the
  # values to be weighted.
  def create_map(country_values)
    output_xml = map_xml
    palette = colour_palette.select do |scale|
      scale.length == number_of_divisions(country_values.values)
    end.first

    country_values.each do |iso_code, value|
      palette_index = value_division(value, country_values.values)

      output_xml.search("##{iso_code} path").each do |path|
        path['style'] = "fill : #{palette[palette_index]};"
      end
    end

    output_xml
  end
end
