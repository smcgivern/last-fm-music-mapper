require 'spec/setup'
require 'lib/thematic_map'

describe 'ThematicMap' do
  it 'should create accessor methods for all class variables' do
    ThematicMap.should.satisfy {|x| x.respond_to?(:map_file)}
    ThematicMap.should.satisfy {|x| x.respond_to?(:colour_palette=)}
  end

  it 'should support alternative spellings of colour' do
    ThematicMap.should.satisfy {|x| x.respond_to?(:color_palette=)}
  end
end
