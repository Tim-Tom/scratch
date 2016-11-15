goal = 34
choices = range(1, 17);
fmt = "%2d"
valid_sizes = dict((x**2, x) for x in range(3,4))
width = valid_sizes[len(choices)]
wm = width - 1;
wp = width + 1

a = [0 for x in choices]
picked = dict((x, False) for x in choices)

class Choose:
    def __init__(self, pos):
        self.pos = pos

    def action(self):
        for n in choices:
            if picked[n]:
                continue
            a[self.pos] = n
            picked[n] = True
            self.next.action()
            picked[n] = False

    def __str__(self):
        return "Choose(%d)" % self.pos

class Decide:
    def __init__(self, pos, *indexes):
        self.pos = pos
        self.indexes = indexes

    def action(self):
        n = goal - sum(a[i] for i in self.indexes)
        if n not in picked or picked[n]:
            return
        a[self.pos] = n
        picked[n] = True
        self.next.action()
        picked[n] = False

    def __str__(self):
        return "Decide(%d, [%s])" % (self.pos, ' '.join(str(i) for i in self.indexes))

class Validate:
    def __init__(self, *indexes):
        self.indexes = indexes

    def action(self):
        calc = sum(a[i] for i in self.indexes)
        if calc == goal:
            self.next.action()

    def __str__(self):
        return "Validate([%s])" % (', '.join(str(i) for i in self.indexes))

class Solution:
    def __init__(self):
        self.count = 0

    def action(self):
        self.count += 1
        print "--- Solution %d ---" % self.count
        for i in range(0, len(choices), width):
            print ' '.join([fmt % v for v in a[i:i+width]])

    def __str__(self):
        return "Solution(Count: %d)" % self.count

if width == 3:
    actions = [
        Choose(0),
        Choose(1),
        Decide(2, 0, 1),
        Choose(4),
        Decide(8, 0, 4),
        Decide(5, 2, 8),
        Decide(3, 4, 5),
        Decide(6, 0, 3),
        Decide(7, 6, 8),
        Validate(1,4,7),
        Validate(2,4,6),
        Solution()
    ]
elif width == 4:
    actions = [
        Choose( 0),
        Choose( 1),
        Choose( 2),
        Decide( 3, 0,1,2),
        Choose( 5),
        Choose( 9),
        Decide(13, 1,5,9),
        Choose(10),
        Decide(15, 0,5,10),
        Choose( 6),
        Decide(14, 2, 6, 10),
        Decide(12, 13,14,15),
        Choose( 4),
        Decide( 7, 4,5,6),
        Decide( 8, 0,4,12),
        Decide(11, 8,9,10),
        Validate(3,7,11,15),
        Validate(3,6,9,12),
        Solution()
    ]

for i in range(len(actions) - 1):
    actions[i].next = actions[i + 1]

actions[0].action()
