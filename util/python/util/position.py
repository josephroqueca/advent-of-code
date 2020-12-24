from copy import copy

class Position:
    _xlim=None
    _ylim=None
    _zlim=None
    _wlim=None
    hexagonal=False

    def __init__(self, x, y, z=None, w=None):
        self.x = x
        self.y = y
        self.z = z
        self.w = w

    def __copy__(self):
        return type(self)(self.x, self.y, self.z, self.w)

    def __attrs(self):
        if self.z is None:
            return (self.x, self.y)
        if self.w is None:
            return (self.x, self.y, self.z)
        return (self.x, self.y, self.z, self.w)

    def __repr__(self):
        return str(self.__attrs())

    def __hash__(self):
        return hash(self.__attrs())

    def __lt__(self, other):
        return self.__attrs() < other.__attrs()

    def __gt__(self, other):
        return self.__attrs() > other.__attrs()

    def __eq__(self, other):
        return isinstance(other, Position) and self.__attrs() == other.__attrs()

    @classmethod
    def set_limits(cls, x, y, z=None, w=None):
        Position._xlim = x
        Position._ylim = y
        Position._zlim = z
        Position._wlim = w

    @classmethod
    def is_within_limits(cls, pos):
        return (Position._xlim is None or pos.x in Position._xlim) and \
            (Position._ylim is None or pos.y in Position._ylim) and \
            (Position._zlim is None or pos.z in Position._zlim) and \
            (Position._wlim is None or pos.w in Position._wlim)

    def adjacent(self):
        if Position.hexagonal:
            return self.hexagonal_adjacent()
        adj = []
        for xd in range(-1, 2):
            for yd in range(-1, 2):
                if self.z is None:
                    if xd == yd == 0: continue
                    adj.append(Position(self.x + xd, self.y + yd))
                    continue
                for zd in range(-1, 2):
                    if self.w is None:
                        if xd == yd == zd == 0: continue
                        adj.append(Position(self.x + xd, self.y + yd, self.z + zd))
                        continue
                    for wd in range(-1, 2):
                        if xd == yd == zd == wd == 0: continue
                        adj.append(Position(self.x + xd, self.y + yd, self.z + zd, self.w + wd))
        return [a for a in adj if Position.is_within_limits(adj)]

    def hexagonal_adjacent(self):
        return [
            self.east(),
            self.west(),
            self.northeast(),
            self.northwest(),
            self.southeast(),
            self.southwest(),
        ]

    def east(self):
        east = copy(self)
        east.move_east()
        return east

    def move_east(self):
        if Position.hexagonal:
            self.x, self.y = self.x - 1, self.y + 1
        else:
            self.x -= 1

    def west(self):
        west = copy(self)
        west.move_west()
        return west

    def move_west(self):
        if Position.hexagonal:
            self.x, self.y = self.x + 1, self.y - 1
        else:
            self.x += 1

    def north(self):
        north = copy(self)
        north.move_north()
        return north

    def move_north(self):
        if Position.hexagonal: return
        self.y -= 1

    def south(self):
        south = copy(self)
        south.move_south()
        return south

    def move_south(self):
        if Position.hexagonal: return
        self.y += 1

    def northwest(self):
        northwest = copy(self)
        northwest.move_northwest()
        return northwest

    def move_northwest(self):
        if not Position.hexagonal: return
        self.x, self.z = self.x + 1, self.z - 1

    def northeast(self):
        northeast = copy(self)
        northeast.move_northeast()
        return northeast

    def move_northeast(self):
        if not Position.hexagonal: return
        self.y, self.z = self.y + 1, self.z - 1

    def southwest(self):
        southwest = copy(self)
        southwest.move_southwest()
        return southwest

    def move_southwest(self):
        if not Position.hexagonal: return
        self.y, self.z = self.y - 1, self.z + 1

    def southeast(self):
        southeast = copy(self)
        southeast.move_southeast()
        return southeast

    def move_southeast(self):
        if not Position.hexagonal: return
        self.x, self.z = self.x - 1, self.z + 1
