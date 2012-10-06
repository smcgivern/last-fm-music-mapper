require './spec/setup'
require './lib/music_mapper'

ISO3 = lambda {|x| x[:iso_3]}

def api_response(f); JSON.parse(open("./spec/fixture/#{f}").read); end

describe 'MusicMapper.hash_keys_to_symbols' do
  it 'should return a hash where string keys are now symbols' do
    MusicMapper.hash_keys_to_symbols({'foo' => 'bar'}).
      should.equal({:foo => 'bar'})
  end

  it 'should work recursively' do
    MusicMapper.hash_keys_to_symbols({'foo' => {'bar' => {'baz' => 'quux'}}}).
      should.equal({:foo => {:bar => {:baz => 'quux'}}})
  end

  it 'should convert hashes inside arrays' do
    MusicMapper.hash_keys_to_symbols([{'foo' => 'bar'}, {'baz' => 'quux'}]).
      should.equal [{:foo => 'bar'}, {:baz => 'quux'}]
  end
end

describe 'MusicMapper.tag_to_countries' do
  def iso_codes(tag)
    MusicMapper.tag_to_countries(tag).map(&ISO3)
  end

  it 'should return a hash containing the ISO 3166 alpha-3 code for the tag' do
    iso_codes('Mali').should.equal ['MLI']
  end

  it 'should search adjectives and country names' do
    iso_codes('British').should.equal ['GBR']
    iso_codes('United Kingdom').should.equal ['GBR']
  end

  it 'should return multiple, sorted results if more than one match is found' do
    iso_codes('Congolese').should.equal ['COG', 'COD']
  end

  it 'should be case-insensitive' do
    iso_codes('MALI').should.equal ['MLI']
    iso_codes('mali').should.equal ['MLI']
  end

  it 'should return an empty array for no matches' do
    iso_codes('Martian').should.equal []
  end
end

describe 'MusicMapper.artist_countries' do
  before do
    @cher = api_response('last_fm_artist_get_top_tags.json')
    @kylie = api_response('last_fm_artist_get_top_tags_kylie_minogue.json')
    @one_tag = api_response('last_fm_artist_get_top_tags_late_junction.json')
    @no_tags = api_response('last_fm_artist_get_top_tags_gb_and_jw.json')
  end

  it 'should return an array of all artist countries' do
    MusicMapper.artist_countries('Kylie Minogue', @kylie).map(&ISO3).
      should.equal ['AUS']
  end

  it 'should only count tags with a count greater than zero' do
    MusicMapper.artist_countries('Cher', @cher).map(&ISO3).
      should.equal ['USA']
  end

  it 'should memoize in the ARTISTS hash' do
    originals = MusicMapper.artist_countries('Cher', @cher).map(&ISO3)

    MusicMapper.artist_countries('Cher', @kylie).map(&ISO3).
      should.equal originals
  end

  it 'should handle artists with only one tag' do
    MusicMapper.artist_countries('One tag', @one_tag).should.equal []
  end

  it 'should handle artists with only no tags' do
    MusicMapper.artist_countries('No tags', @no_tags).should.equal []
  end
end

describe 'MusicMapper.user_artists' do
  def user_artists(p, j); MusicMapper.user_artists('rj', p, 0, j); end

  before do
    module MusicMapper
      class << self
        alias :artist_countries_original :artist_countries

        def artist_countries(*a); []; end
      end
    end

    @overall = api_response('last_fm_user_get_top_artists.json')
    @sevenday = api_response('last_fm_user_get_top_artists_7day.json')
  end

  after do
    module MusicMapper
      class << self
        alias :artist_countries :artist_countries_original
      end
    end
  end

  it 'should load artists for the period specified' do
    artists = user_artists('7day', @sevenday)

    artists.length.should.satisfy {|x| x > 10}
    artists.first[:playcount].should.equal 20
  end

  it 'should memoize in the users hash' do
    originals = user_artists('overall', @overall)

    user_artists('overall', @sevenday).should.equal originals
  end

  it 'should use the medium-sized artist image' do
    user_artists('overall', @overall)

    MusicMapper::ARTISTS['Dream Theater'][:image].
      should.equal 'http://userserve-ak.last.fm/serve/64/5623420.jpg'
  end

  it 'should include artist info in the return value' do
    user_artists('overall', @overall).first[:image].
      should.equal 'http://userserve-ak.last.fm/serve/64/5623420.jpg'
  end

  it 'should sort artists from highest to lowest playcount' do
    playcounts = user_artists('overall', @overall).map {|c| c[:playcount]}

    playcounts.should.equal playcounts.sort.reverse
  end
end

describe 'MusicMapper.group_by_country' do
  before do
    @user_artists = api_response('user_artists.json').map do |artist|
      MusicMapper.hash_keys_to_symbols(artist)
    end

    @grouped_by_country = MusicMapper.group_by_country(@user_artists)
  end

  it 'should collect user artists by country' do
    @grouped_by_country.map(&ISO3).sort[0..4].
      should.equal ['AUS', 'CAN', 'DEU', 'GBR', 'ITA']
  end

  it 'should calculate the playcount by country' do
    @grouped_by_country.select {|c| c[:iso_3] == 'USA'}.first[:playcount].
      should.equal 35
  end

  it 'should put artists in every country they belong to' do
    @grouped_by_country.select do |country|
      country[:artists].select {|a| a[:name] == 'Grace Jones'}.length > 0
    end.
      map(&ISO3).
      sort.
      should.equal ['JAM', 'USA']
  end

  it 'should sort countries from highest to lowest playcount' do
    playcounts = @grouped_by_country.map {|c| c[:playcount]}

    playcounts.should.equal playcounts.sort.reverse
  end
end
