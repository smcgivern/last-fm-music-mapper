module Kramdown
  include Haml::Filters::Base

  def render(text)
    ::Kramdown::Document.new(text).to_html
  end
end

set :haml, {:format => :html5}
set :views, "#{File.dirname(__FILE__)}/view"
set :static_cache_control, [:public, :max_age => 86400]

helpers do
  include Rack::Utils

  alias_method :h, :escape_html

  # URL relative to root as defined in SETTINGS['root'].
  def r(s)
    return s unless (s =~ /^\// && !(s =~ /^\/#{SETTINGS['root']}/))

    "/#{SETTINGS['root']}#{s}"
  end

  # Absolute URL to s.
  def a(s); request.url.split('/')[0..2].join('/') + r(s); end

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
