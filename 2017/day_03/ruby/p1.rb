# frozen_string_literal: true

input = 347_991

down = 1
right = 0
even = 2

while (even + 2)**2 < input
  even += 2
  down += 1
  right += 1
end

max_down = down + 1
max_right = right + 1
direction = 'left'

target = even**2
while target != input
  if direction == 'left' && right < max_right
    target += 1
    right += 1
    direction = 'down' if right == max_right
  elsif direction == 'down' && down > -(max_down - 1)
    target += 1
    down -= 1
    direction = 'right' if down == -(max_down - 1)
  elsif direction == 'right' && right > -(max_right + 1)
    target += 1
    right -= 1
    direction = 'up' if right == -(max_right + 1)
  elsif direction == 'up' && down < max_down
    target += 1
    down += 1
    direction = 'left' if down == max_down
  end
end

steps = down.abs + right.abs
puts format('%<steps>d steps are required', steps: steps)
