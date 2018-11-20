input = 347991

down = 1
right = 0
even = 2

while (even + 2) ** 2 < input do
    even += 2
    down += 1
    right += 1
end

maxDown = down + 1
maxRight = right + 1
direction = 'left'

target = even ** 2
while target != input do
    if direction == 'left' && right < maxRight then
        target += 1
        right += 1
        if right == maxRight then direction = 'down' end
    elsif direction == 'down' && down > -(maxDown - 1) then
        target += 1
        down -= 1
        if down == -(maxDown - 1) then direction = 'right' end
    elsif direction == 'right' && right > -(maxRight + 1) then
        target += 1
        right -= 1
        if right == -(maxRight + 1) then direction = 'up' end
    elsif direction == 'up' && down < maxDown then
        target += 1
        down += 1
        if down == maxDown then direction = 'left' end
    end
end

puts down.abs + right.abs