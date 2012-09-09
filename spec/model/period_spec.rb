require './spec/setup'
require './model/schema'

describe 'Period' do
  it 'should find the period from the key' do
    Period('overall').should.equal Period.get('overall')
  end

  it 'should pass Period objects through' do
    period = Period('7day')

    Period(period).should.equal period
  end
end
