# frozen_string_literal: true

input = 347_991

squares = Array.new(1) { Array.new(1, 1) }
direction = 'left'
x = 0
y = 0
array_width = 1
array_height = 1

def expand_width!(squares)
  squares.each do |row|
    row << 0
    row << 0
    (row.size - 1).downto(1) do |index|
      row[index] = row[index - 1]
    end
    row[0] = 0
  end
end

def expand_height!(squares)
  squares << Array.new(width, 0)
  squares << Array.new(width, 0)
  (squares.size - 1).downto(1) do |index|
    squares[index] = squares[index - 1]
  end
  squares[0] = Array.new(squares[0].size, 0)
end

def expand_squares!(squares, width, height)
  expand_width!(squares)
  width += 2

  expand_height!(squares)
  height += 2

  [width, height]
end

while squares[y][x] < input
  if direction == 'left' && x < array_width
    x += 1
    if x == array_width
      array_width, array_height = expand_squares!(squares, array_width, array_height)
      x += 1
      y += 1
      direction = 'up'
    end
  elsif direction == 'up' && y.positive?
    y -= 1
    direction = 'right' if y.zero?
  elsif direction == 'right' && x.positive?
    x -= 1
    direction = 'down' if x.zero?
  elsif direction == 'down' && y < array_height - 1
    y += 1
    direction = 'left' if y == array_height - 1
  end

  squares[y][x] = (y.positive? ? squares[y - 1][x] : 0) +
                  (x.positive? ? squares[y][x - 1] : 0) +
                  (y < array_height - 1 ? squares[y + 1][x] : 0) +
                  (x < array_width - 1 ? squares[y][x + 1] : 0) +
                  (y.positive? && x.positive? ? squares[y - 1][x - 1] : 0) +
                  (y.positive? && x < array_width - 1 ? squares[y - 1][x + 1] : 0) +
                  (y < array_height - 1 && x.positive? ? squares[y + 1][x - 1] : 0) +
                  (y < array_height - 1 && x < array_width - 1 ? squares[y + 1][x + 1] : 0)
end

puts format('The first larger value is %<value>d', value: squares[y][x])
