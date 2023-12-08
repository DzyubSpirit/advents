import sys

with open(sys.argv[1]) as f:
    lines = [l.strip() for l in f.readlines()]


def is_game_possible(game: str) -> bool:
    sets = game[game.find(':')+2:].split('; ')
    for s in sets:
        num_colors = s.split(', ')
        for num_color in num_colors:
            n, color = num_color.split(' ')
            n = int(n)
            if color == 'red' and n > 12:
                return False
            if color == 'green' and n > 13:
                return False
            if color == 'blue' and n > 14:
                return False
    return True


possible_games_sum = 0
for i, line in enumerate(lines):
    if is_game_possible(line):
        possible_games_sum += i + 1
print(possible_games_sum)
