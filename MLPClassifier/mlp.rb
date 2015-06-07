require 'csv'

srand(1234)

HOW_SMALL = 10
LEARNING_RATE = 0.2
MAX_COL = 100

class Preprocessing
  def self.getFromCSV(name)
    data = []
    CSV.foreach(name, converters: :float , headers: false) do |tData|
      row = []
      for i in 0..MAX_COL
        if tData[i]
          row << tData[i]
        end
      end
      if row[0]
        data << row
      end
    end
    return data
  end
  def self.sample(data)
    return data.shuffle()
  end
  def self.partition(data, percent, isFirst)
    l = ((data.length - 1) * percent).floor
    return isFirst ? data[0..l] : data[(l+1)..data.length - 1]    
  end
  def self.nomDataToColumns(data, colNum)
    newData = []
    newCol = []
    values =  (data.map {|row| colNum}).uniq
    for i in 0..(values.length - 1)
      for j in 0..(data[colNum].length - 1)
        newCol[j] = data[colNum][j] == values[i]
        end
      newData << newCol
    end
    return data[0..(colNum - 1)] + newData + data[(colNum + 1)..(data.length - 1)]
  end
  def self.ordDataToInt(data, colNum, order)
    for i in 0..(data.length - 1)
      data[i][colNum] = order.index(data[i][colNum])
    end
    return data
  end
end

class Attribute
  @weight
  @lastInput
  def initialize()
    @weight = rand() / (rand(HOW_SMALL) + 1) * ((rand(2) + 1) % 2 == 0 ? 1 : -1)
  end
  def changeWeight(d)
    @weight -= LEARNING_RATE * d * @lastInput 
  end
  def getValue(i)
    @lastInput = i
    return i * @weight
  end
  def weight
    return @weight
  end
end

class Neuron
  @attributes
  @activation
  def sumInputs(inputs)
    sum = 0
    for i in 0..(@attributes.length - 1)
      sum += @attributes[i].getValue(inputs[i])
    end
    return sum
  end
  def activation(inputs)
    if(!@attributes)
      @attributes = []
      for i in 0..(inputs.length - 1)
        @attributes[i] = Attribute.new()
      end
    end
    @activation = 1 / (1 + Math.exp(-sumInputs(inputs)))
    return @activation
  end
  def back(d)
    outputs = []
    for i in 0..(@attributes.length - 1)
      @attributes[i].changeWeight(d)
      outputs << @attributes[i].getValue(d)
    end
    return outputs
  end
  def a
    return @activation
  end
end

class Layer
  @neurons
  @targets
  def initialize(numNeurons, targets)
    @neurons = []
    for i in 0..(numNeurons - 1)
      @neurons[i] = Neuron.new()
    end
    @targets = targets
  end
  def forward(inputs)
    outputs = []
    for i in 0..(@neurons.length - 1)
      outputs << @neurons[i].activation(inputs)
    end
    return outputs
  end
  def back(inputs)
    outputs = []
    d = 0
    for i in 0..(@neurons.length - 1)
      a = @neurons[i].a
      if(@targets)
        t = inputs[inputs.length - 1] == @targets[i] ? 1 : 0
        d = a * (1 - a) * (a - t)
      else
        sum = 0
        for k in 0..(inputs.length - 1)
          sum += inputs[k][i]
        end
        d = a * (1 - a) * sum
      end
      outputs << @neurons[i].back(d)
    end
    return outputs
  end
  def target(i)
    return @targets[i]
  end
end

class Network
  @hLayers
  @oLayer
  def initialize(neuronsInLayers, classes)
    @hLayers = []
    for i in 0..(neuronsInLayers.length - 1)
      @hLayers << Layer.new(neuronsInLayers[i], nil)
    end
    outNodes = classes.length
    targets = []
    for i in 0..(outNodes - 1)
      targets << classes[i % classes.length]
    end
    @oLayer = Layer.new(outNodes, targets)
  end
  def train(inputs, numCycles)
    out = 0
    returns = []
    for i in 1..numCycles
      cnt = 0
      for j in 0..(inputs.length - 1)
        out = @hLayers[0].forward([-1] + inputs[j][0..(inputs[j].length - 2)])
        for k in 1..(@hLayers.length - 1)
          out = @hLayers[k].forward([-1] + out)
        end
        out = @oLayer.forward([-1] + out)
        if(@oLayer.target(out.index(out.max)) == inputs[j].last)
          cnt += 1
        end        
        out = @oLayer.back(inputs[j])
        for k in (0..(@hLayers.length - 1)).reverse_each
          out = @hLayers[k].back(out)
        end
      end
      returns << ((cnt.to_f/inputs.length.to_f) * 100)
      if(returns.last >= 98)
        break
      end
    end
    return returns
  end
  def classify(data)
    out = @hLayers[0].forward([-1] + data)
    for k in 1..(@hLayers.length - 1)
      out = @hLayers[k].forward([-1] + out)
    end
    out = @oLayer.forward([-1] + out)
    c = @oLayer.target(out.index(out.max))
    return c 
  end
  def test(testData)
    total = 0
    for i in 0..(testData.length - 1)
      c = classify(testData[i])
      if(c != testData[i][testData[i].length - 1])
        #puts "#{c} != #{testData[i][testData[i].length - 1]}"
      else
        #puts "#{c} == #{testData[i][testData[i].length - 1]}"
        total += 1
      end
    end
    puts "It is #{(total.to_f/testData.length.to_f) * 100}% Correct"
  end
end


#=begin
outfile = "carOutput.csv"
data = Preprocessing.getFromCSV("../car.data")
data = Preprocessing.ordDataToInt(data, 0, ["low", "med", "high", "vhigh"])
data = Preprocessing.ordDataToInt(data, 1, ["low", "med", "high", "vhigh"])
data = Preprocessing.ordDataToInt(data, 4, ["small", "med", "big"])
data = Preprocessing.ordDataToInt(data, 2, [0,1,2,3,4,"5more"])
data = Preprocessing.ordDataToInt(data, 3, [0,1,2,3,4,"more"])
data = Preprocessing.ordDataToInt(data, 5, ["low", "med", "high", "vhigh"])
#=end

#=begin
outfile = "irisOutput.csv"
data = Preprocessing.getFromCSV("../iris.data")
#=end

=begin
outfile = "diabetesOut.csv"
data = Preprocessing.getFromCSV("../diabetes.data")
=end

data = Preprocessing.sample(data)

classifications =  (data.map {|row| row[data[0].length - 1]}).uniq

trainData = Preprocessing.partition(data, 0.7, true)
testData  = Preprocessing.partition(data, 0.7, false)

neuronsInLayers = [5]

mlp = Network.new(neuronsInLayers, classifications)
out = mlp.train(trainData, 500)
mlp.test(testData)

#=begin
CSV.open(outfile, "w") do |csv| 
  csv << out
end
#=end

