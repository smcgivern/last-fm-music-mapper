require 'spec/setup'
require 'lib/thematic_map'

describe 'ThematicMap::Config' do
  it 'should create accessor methods for all class variables' do
    ThematicMap::Config.should.satisfy {|x| x.respond_to?(:map_file)}
    ThematicMap::Config.should.satisfy {|x| x.respond_to?(:colour_palette=)}
  end

  it 'should support alternative spellings of colour' do
    ThematicMap::Config.should.satisfy {|x| x.respond_to?(:color_palette=)}
  end
end
