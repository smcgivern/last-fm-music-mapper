require 'addressable/template'
require 'digest/sha1'
require 'fileutils'
require 'json'
require 'open-uri'

# A minimal implementation of the read-only Last.fm 2.0 API methods. Handles
# rate limiting as per the terms of service (no more than 5 requests / second).
#
module LastFM
  class Config
    @@api_domain = 'ws.audioscrobbler.com'
    @@api_version = '2.0'
    @@api_format = 'json'
    @@api_key = 'Overwrite when using module'

    @@requests_per_minute = 300
    @@recent_requests = []

    @@cache_for = 7 * 24 * 60 * 60
    @@cache_directory = nil

    # Create accessors for class variables (used by all subclasses, so can be
    # set anywhere).
    #
    self.class_variables.each do |var|
      (class << self; self; end).class_eval do
        define_method("#{var[2..-1]}") {class_variable_get(var)}
        define_method("#{var[2..-1]}=") {|x| class_variable_set(var, x)}
      end
    end
  end

  # Addressable templates for each method's parameters defined in PARAMS below.
  #
  TEMPLATES = {}

  # Method parameters. Apart from :base, which is a special case, the first
  # level of the hash represents a Last.fm class (user, tag, etc.). A
  # corresponding Ruby class will be created with methods available as in the
  # hash.
  #
  # Note: some methods require an artist name /or/ a MusicBrainz ID. At the
  # moment, they're both optional here, but the request will fail if neither is
  # supplied.
  #
  PARAMS = {
    :base => {
      :required => [:method, :api_key, :format],
      :optional => [],
    },
    :user => {
      :get_info => {
        :required => [:user],
      },
      :get_top_artists => {
        :required => [:user],
        :optional => [:period, :limit, :page],
      },
    },
    :artist => {
      :get_info => {
        :optional => [:artist, :mbid, :lang, :autocorrect, :username],
      },
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
      if cache_directory
        FileUtils.mkdir_p(cache_directory)

        cache_file = File.join(cache_directory,
                               Digest::SHA1.hexdigest(address))

        if File.exist?(cache_file)
          if File.mtime(cache_file) > (Time.now - cache_for)
            response = open(cache_file).read
          else
            File.delete(cache_file)
          end
        end
      end

      unless response
        limit_rate

        response = open(address).read

        raise(Exception, response['message']) if response['error']

        open(cache_file, 'w').puts(response) if cache_directory
      end

      JSON.parse(response)
    end

    def self.build_request(method, template, allowed_params, params={})
      required_params = (allowed_params[:required] || [])
      optional_params = (allowed_params[:optional] || [])

      default_params = {
        :domain => api_domain,
        :path => [api_version],
        :method => method,
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

    def self.metaclass
      class << self
        self
      end
    end
  end

  PARAMS.each do |klass, methods|
    next if klass == :base

    # Create class named after klass (upper-casing first letter), inheriting
    # from Base, and with no methods of its own.
    #
    class_name = klass.to_s.gsub(/\b\w/){ $&.upcase }

    const_set(class_name, Class.new(Base))

    TEMPLATES[klass] ||= {}

    methods.each do |method, params|
      merged_params = params.clone

      PARAMS[:base].each do |type, param|
        merged_params[type] = [param, params[type]].flatten.compact
      end

      required = merged_params[:required].map {|x| "#{x}={#{x}}"}.join('&')
      optional = "{-join|&|#{merged_params[:optional].join(',')}}"
      template = "http://{domain}/{-suffix|/|path}?#{required}&#{optional}"

      TEMPLATES[klass][method] = Addressable::Template.new(template)

      # Define class method, calling LastFM::Base.build_request.
      const_get(class_name).metaclass.send(:define_method, method) do |*args|
        build_request("#{klass}.#{method.to_s.gsub('_', '')}",
                      TEMPLATES[klass][method],
                      params,
                      *args)
      end
    end
  end
end
