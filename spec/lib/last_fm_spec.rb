require 'spec/setup'
require 'lib/last_fm'

describe 'LastFM::Config' do
  it 'should create accessor methods for all class variables' do
    LastFM::Config.should.satisfy {|x| x.respond_to?(:api_domain)}
    LastFM::Config.should.satisfy {|x| x.respond_to?(:api_version=)}
  end

  it 'should propogate changes to all subclasses' do
    new_api_key = (LastFM::Config.api_key = 'jfmamjjasond')

    LastFM::User.api_key.should.equal new_api_key
  end
end

describe 'LastFM::Base.limit_rate' do
  before do
    @base = LastFM::Base.clone

    @base.recent_requests = []
    @base.instance_variable_set(:@sleep, 0)

    def @base.sleep t; Kernel::sleep(t); @sleep += t; end
  end

  it 'should sleep if there are too many recent requests' do
    1.upto(@base.requests_per_minute) do |i|
      @base.recent_requests << Time.now - 59
    end

    @base.limit_rate
    @base.sleep(0).should.satisfy {|x| x > 0}
  end

  it 'should not sleep with fewer requests' do
    3.upto(@base.requests_per_minute) {|i| @base.recent_requests << Time.now}

    @base.limit_rate
    @base.sleep(0).should.equal 0
  end

  it 'should disregard requests from more than a minute ago' do
    @base.recent_requests << (Time.now - 120)
    3.upto(@base.requests_per_minute) {|i| @base.recent_requests << Time.now}

    @base.limit_rate
    @base.sleep(0).should.equal 0
  end
end

describe 'LastFM::Base.method_missing' do
  it 'should raise an ArgumentError if there are no methods for the class' do
    lambda {LastFM::Base.not_an_api_call}.should.raise(ArgumentError).
      message.should.match(/No methods found/)
  end

  it 'should raise an ArgumentError if the method is for another class' do
    lambda {LastFM::Artist.get_top_artists}.should.raise(ArgumentError).
      message.should.match(/No method get_top_artists/)
  end

  it 'should raise an ArgumentError if a required parameter is missing' do
    lambda {LastFM::User.get_top_artists}.should.raise(ArgumentError).
      message.should.match(/Missing required parameters/)
  end

  it 'should allow the base parameters to be overwritten' do
  end
end
