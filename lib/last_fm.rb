require 'addressable/template'
require 'json'
require 'open-uri'

# A minimal implementation of the read-only Last.fm 2.0 API methods. Handles
# rate limiting as per the terms of service (no more than 5 requests / second).
module LastFM
  class Config
    @@api_domain = 'ws.audioscrobbler.com'
    @@api_version = '2.0'
    @@api_format = 'json'
    @@api_key = 'Overwrite when using module'

    @@requests_per_minute = 300
    @@recent_requests = []

    # Create accessors for class variables (used by all subclasses, so
    # can be set anywhere.
    self.class_variables.each do |var|
      (class << self; self; end).class_eval do
        define_method("#{var[2..-1]}") {class_variable_get(var)}
        define_method("#{var[2..-1]}=") {|x| class_variable_set(var, x)}
      end
    end
  end

  # Addressable templates for each method's parameters defined in PARAMS below.
  TEMPLATES = {}

  # Method parameters. Apart from :base, which is a special case, the first
  # level of the hash represents a Last.fm class (user, tag, etc.). A
  # corresponding Ruby class will be created with methods available as in the
  # hash.
  PARAMS = {
    :base => {
      :required => [:method, :api_key, :format],
    },
    :user => {
      :get_top_artists => {
        :required => [:user],
        :optional => [:period, :limit, :page],
      },
    },
    :artist => {
      :get_top_tags => {
        :optional => [:artist, :mbid, :autocorrect],
      },
    },
  }

  class Base < Config
    def self.limit_rate
      loop do
        # Remove requests from more than a minute ago.
        recent_requests.reject! {|t| (Time.now - t) > 60}

        break if recent_requests.length < requests_per_minute

        sleep(1)
      end

      recent_requests << Time.now
    end

    def self.api_request(address)
      limit_rate

      JSON.parse(open(address).read)
    end

    def self.method_missing(method, params={})
      klass = self.name.split('::').last.downcase

      unless (klass != 'base' and methods = PARAMS[klass.to_sym])
        raise(ArgumentError, "No methods found for class #{klass}")
      end

      unless (allowed_params = methods[method.to_sym])
        raise(ArgumentError, "No method #{method} found for class #{klass}")
      end

      template = TEMPLATES[klass.to_sym][method.to_sym]
      required_params = allowed_params[:required]
      optional_params = allowed_params[:optional]

      default_params = {
        :domain => api_domain,
        :path => [api_version],
        :method => "#{klass}.#{method.to_s.gsub('_', '')}",
        :api_key => api_key,
        :format => api_format,
      }

      missing_params = required_params.select {|k| !params.keys.include?(k)}

      excess_params = params.keys.select do |key|
        !(required_params + optional_params + default_params.keys).include?(key)
      end

      unless missing_params.empty?
        raise(ArgumentError,
              "Missing required parameters: #{missing_params.join(', ')}")
      end

      api_request(template.expand(default_params.merge(params)))
    end
  end

  PARAMS.each do |klass, methods|
    next if klass == :base

    # Create class named after klass (upper-casing first letter), inheriting
    # from Base, and with no methods of its own.
    self.const_set(klass.to_s.gsub(/\b\w/){ $&.upcase }, Class.new(Base))

    TEMPLATES[klass] ||= {}

    methods.each do |method, params|
      params = params.clone

      PARAMS[:base].each {|t, p| params[t] = [p, params[t]].flatten.compact}

      required = params[:required].map {|x| "#{x}={#{x}}"}.join('&')
      optional = "{-join|&|#{params[:optional].join(',')}}"
      template = "http://{domain}/{-suffix|/|path}?#{required}&#{optional}"

      TEMPLATES[klass][method] = Addressable::Template.new(template)
    end
  end
end
