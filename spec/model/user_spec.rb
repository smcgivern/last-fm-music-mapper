require './spec/setup'
require './model/schema'

shared 'User' do
  before do
    @primary = './spec/fixture/last_fm_user_get_top_artists.json'
    @alternate = './spec/fixture/last_fm_user_get_top_artists_7day.json'
  end

  def user(username='rj')
    User.first_or_create(:username => username).
      load_artists(JSON.parse(open(@primary).read))
  end
end

describe 'User.load_artists' do
  behaves_like 'User'

  it 'should load artists from a hash' do
    user.user_artists.should.satisfy {|x| x.length > 0}
  end

  it 'should skip loading if this period was loaded recently' do
    user_artists = user.user_artists
    alternate = JSON.parse(open(@alternate).read)

    alternate['topartists']['@attr']['type'] = 'overall'

    user.load_artists(alternate).user_artists.
      should.equal user_artists
  end

  it "should load if the user wasn't updated for the period recently" do
    user_artists = user.user_artists('overall')

    user.load_artists(JSON.parse(open(@alternate).read)).
      user_artists('7day').
      should.satisfy {|x| x.length < user_artists.length}
  end

  it 'should update artist details every time' do
    user = user('rj-2')
    mbid = '614e3804-7d34-41ba-857f-811bad7c2b7a'
    alternate = JSON.parse(open(@alternate).read)

    alternate['topartists']['artist'] =
      alternate['topartists']['artist'].map do |artist|

      artist['name'] = 'Dyer Straights' if artist['mbid'] == mbid

      artist
    end

    Artist.get(mbid).name.should.equal 'Dire Straits'

    user.load_artists(alternate)

    Artist.get(mbid).name.should.equal 'Dyer Straights'
  end

  it 'should use the medium-sized image' do
    Artist.get('28503ab7-8bf2-4666-a7bd-2644bfc7cb1d').image.
      should.equal 'http://userserve-ak.last.fm/serve/64/5623420.jpg'
  end

  it 'should return the user' do
    user('rj-3').class.should.be.same_as User
  end
end

describe 'User.user_artists' do
  behaves_like 'User'

  before do
    @user = user('rj-user_artists').
      load_artists(JSON.parse(open(@primary).read)).
      load_artists(JSON.parse(open(@alternate).read))
  end

  it 'should return user artists based on the period' do
    @user.user_artists('overall').should.not.equal @user.user_artists('7day')
  end

  it "should default to the 'overall' period" do
    @user.user_artists('overall').should.equal @user.user_artists
  end
end
