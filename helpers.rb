set :haml, {:format => :html5}
set :views, "#{File.dirname(__FILE__)}/view"

helpers do
  include Rack::Utils

  alias_method :h, :escape_html

  def country_names(countries)
    countries.map do |c|
      "<em class=\"country-#{c[:iso_3]} country-name\">#{c[:name]}</em>"
    end
  end

  def country_classes(cs); cs.map {|c| "country-#{c[:iso_3]}"}; end

  def sentence_join(array)
    return array.join(' and ') if array.length <= 2

    [array[0...-1].join(', '),', and ', array[-1]].join
  end
end
