input = 347991

squares = Array.new(1){Array.new(1, 1)}
direction = 'left'
x = 0
y = 0
arrayWidth = 1
arrayHeight = 1

def expandSquares!(squares, width, height)
    squares.each do |row|
        row << 0
        row << 0
        (row.size - 1).downto(1) do |index|
            row[index] = row[index - 1]
        end
        row[0] = 0
    end
    width += 2

    squares << Array.new(width, 0)
    squares << Array.new(width, 0)
    (squares.size - 1).downto(1) do |index|
        squares[index] = squares[index - 1]
    end
    squares[0] = Array.new(squares[0].size, 0)
    height += 2

    return width, height
end

while squares[y][x] < input do
    if direction == 'left' && x < arrayWidth then
        x += 1
        if x == arrayWidth then
            arrayWidth, arrayHeight = expandSquares!(squares, arrayWidth, arrayHeight)
            x += 1
            y += 1
            direction = 'up'
        end
    elsif direction == 'up' && y > 0 then
        y -= 1
        if y == 0 then direction = 'right' end
    elsif direction == 'right' && x > 0 then
        x -= 1
        if x == 0 then direction = 'down' end
    elsif direction == 'down' && y < arrayHeight - 1 then
        y += 1
        if y == arrayHeight - 1 then direction = 'left' end
    end

    squares[y][x] = (y > 0 ? squares[y - 1][x] : 0) +
                    (x > 0 ? squares[y][x - 1] : 0) +
                    (y < arrayHeight - 1 ? squares[y + 1][x] : 0) +
                    (x < arrayWidth - 1 ? squares[y][x + 1] : 0) +
                    (y > 0 && x > 0 ? squares[y - 1][x - 1] : 0) +
                    (y > 0 && x < arrayWidth - 1 ? squares[y - 1][x + 1] : 0) +
                    (y < arrayHeight - 1 && x > 0 ? squares[y + 1][x - 1] : 0) +
                    (y < arrayHeight - 1 && x < arrayWidth - 1 ? squares[y + 1][x + 1] : 0)
end

puts squares[y][x]
