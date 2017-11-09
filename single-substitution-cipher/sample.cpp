#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <vector>

const char* const cipher_text = \
    "The rain in spain falls mainly on the plains\n" \
    "The quick brown fox jumps over the lazy dog\n" \
    "How now brown cow\n" \
    "Jack and Jill ran up the hill to fetch a pail of water\n" \
    "Jack fell down and broke his crown and Jill came tumbling after\n";

typedef struct pattern_range {
    int from;
    int to;
} pattern_range;

const int num_letters = 26;
const char* const letters = "anoilerwcfthpsmubdjkgyqvxz";

#define NUM_PATTERNS 9
#define MAX_PATTERN_SIZE 8
const char* const patterns[NUM_PATTERNS] = {"a", "ab", "abc", "abcc", "abcd", "abccd", "abcde", "abcdef", "abcdefgh"};
const pattern_range pattern_index[MAX_PATTERN_SIZE + 1] = {{-1,-1}, {0,0}, {1,1}, {2,2}, {3,4}, {5,6}, {7,7}, {-1,-1}, {8,8}};

typedef struct word {
    const char* const word;
    const char* const pattern;
} word_t;

#define NUM_WORDS 38
const word_t words[NUM_WORDS] = {{"a", patterns[0]}, {"in", patterns[1]}, {"of", patterns[1]}, {"on", patterns[1]}, {"to", patterns[1]}, {"up", patterns[1]}, {"and", patterns[2]}, {"cow", patterns[2]}, {"dog", patterns[2]}, {"fox", patterns[2]}, {"his", patterns[2]}, {"how", patterns[2]}, {"now", patterns[2]}, {"ran", patterns[2]}, {"the", patterns[2]}, {"came", patterns[4]}, {"down", patterns[4]}, {"fell", patterns[3]}, {"hill", patterns[3]}, {"jack", patterns[4]}, {"jill", patterns[3]}, {"lazy", patterns[4]}, {"over", patterns[4]}, {"pail", patterns[4]}, {"rain", patterns[4]}, {"after", patterns[6]}, {"broke", patterns[6]}, {"brown", patterns[6]}, {"crown", patterns[6]}, {"falls", patterns[5]}, {"fetch", patterns[6]}, {"jumps", patterns[6]}, {"quick", patterns[6]}, {"spain", patterns[6]}, {"water", patterns[6]}, {"mainly", patterns[7]}, {"plains", patterns[7]}, {"tumbling", patterns[8]}};

typedef std::vector<const char*> wordList;
typedef std::map<const char*, wordList> wordMap;

wordMap wordsByPattern;

typedef struct charPossibilities {
  char c;
  int numPossible;
  char possibilities[26];
} charPossibilities;

typedef struct wordPossibilities {
  bool isUsingRoot;
  wordList* words;
} wordPossibilities;

typedef struct state {
  charPossibilities characters[NUM_LETTERS];
  wordPossibilities words[NUM_WORDS];
} state;

void make_pattern(const char* buffer, char* pattern) {
  char seen[26] = {0};
  char letter = 'a';
  const char* from = buffer;
  char* to = pattern;
  while(*from != '\0') {
    int i = *from - 'a';
    if (!seen[i]) {
      seen[i] = letter++;
    }
    *to++ = seen[i];
    ++from;
  }
  *to = '\0';
}

void build_word_list(const char* const filename) {
  FILE* input = fopen(filename, "r");
  char buffer[128];
  char pattern[128];
  int len, i;
  pattern_range range;
  const char * cloned;
  if (!input) {
    fprintf(stderr, "Failed to open word file list");
    exit(1);
  }
  for (i = 0; i < NUM_PATTERNS; ++i) {
    wordList dummy;
    wordsByPattern.insert(std::make_pair(patterns[i], dummy));
  }
  while(!feof(input)) {
    if (!fgets(buffer, 128, input)) {
      break;
    }
    len = strlen(buffer) - 1;
    buffer[len] = '\0';
    if (len > MAX_PATTERN_SIZE || pattern_index[len].from == -1) {
      continue;
    }
    make_pattern(buffer, pattern);
    range = pattern_index[len];
    for(i = range.from; i <= range.to; ++i) {
      if (strcmp(pattern, patterns[i]) == 0) {
        goto found;
      }
    }
    continue;
  found:
    cloned = strdup(buffer);
    wordsByPattern[patterns[i]].push_back(cloned);
  }
}

int main(int argc, const char* const argv[]) {
  build_word_list("words.txt");
  return 0;
}
