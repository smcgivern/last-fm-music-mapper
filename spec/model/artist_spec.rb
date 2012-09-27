require './spec/setup'
require './model/schema'

describe 'Artist.load_tags' do
  before do
    @primary = './spec/fixture/last_fm_artist_get_top_tags.json'
    @alternate = './spec/fixture/last_fm_artist_get_top_tags_madonna.json'
  end

  def artist(name='Cher')
    Artist.first_or_create(:music_brainz_id => name, :name => name).
      load_tags(JSON.parse(open(@primary).read))
  end

  it 'should load tags from a hash' do
    artist.tags.should.satisfy {|x| x.length > 0}
  end

  it "should skip loading if this artists's tags are up to date" do
    tags = artist.tags
    alternate = JSON.parse(open(@alternate).read)

    alternate['toptags']['@attr']['name'] = 'Cher'

    artist.load_tags(alternate).tags.should.equal tags
  end

  it "should skip loading if the artist name doesn't match" do
    artist('Cyndi Lauper').tags.should.equal []
  end

  it "should load if this artists's tags aren't up to date" do
    tags = artist.tags

    artist('Madonna').load_tags(JSON.parse(open(@alternate).read)).tags.
      should.satisfy {|x| x != tags}
  end

  it 'should return the artist' do
    artist('Kylie Minogue').class.should.be.same_as Artist
  end
end

describe 'Artist::make_mbid' do
  it 'should use the MusicBrainz ID if it exists' do
    Artist.make_mbid({'mbid' => 'wpujc-vaas', 'name' => 'Chaminda'}).
      should.equal 'wpujc-vaas'
  end

  it 'should generate an MD5 hash from the name otherwise' do
    Artist.make_mbid({'mbid' => '', 'name' => ''}).
      should.equal 'md5:d41d8cd98f00b204e9800998ecf8427e'
  end

  it 'should generate a value no more than 36 characters long' do
    Artist.make_mbid({'mbid' => '7e35659f-f3bb-4c28-bcf5-47bd5cfa2bca'}).length.
      should.equal 36

    Artist.make_mbid({'mbid' => '', 'name' => ''}).length.
      should.equal 36
  end
end
