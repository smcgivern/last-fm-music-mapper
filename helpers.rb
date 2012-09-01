set :haml, {:format => :html5}
set :views, "#{File.dirname(__FILE__)}/view"

helpers do
  include Rack::Utils
  alias_method :h, :escape_html
end
