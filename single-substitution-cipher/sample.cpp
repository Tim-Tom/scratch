#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <vector>

#define NUM_LETTERS 26
#define NUM_WORDS 38
#define NUM_PATTERNS 9
#define MAX_PATTERN_SIZE 8


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

const char* const letters = "anoilerwcfthpsmubdjkgyqvxz";
const int letter_mapping[26] = {0, 16, 8, 17, 5, 9, 20, 11, 3, 18, 19, 4, 14, 1, 2, 12, 22, 6, 13, 10, 15, 23, 7, 24, 21, 25};

const char* const patterns[NUM_PATTERNS] = {"a", "ab", "abc", "abcc", "abcd", "abccd", "abcde", "abcdef", "abcdefgh"};
const pattern_range pattern_index[MAX_PATTERN_SIZE + 1] = {{-1,-1}, {0,0}, {1,1}, {2,2}, {3,4}, {5,6}, {7,7}, {-1,-1}, {8,8}};

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

typedef struct word word_t;

typedef bool (*checkWord_t)(const state&, const word_t&, const char*);

struct word {
  const char* const word;
  const char* const pattern;
  checkWord_t checkWord;
};

bool check_letter(const state& s, char c, int ci) {
  int j;
  for (j = 0; j < s.characters[ci].numPossible; ++j) {
    if (s.characters[ci].possibilities[j] == c) {
      return true;
    }
  }
  return false;
}

bool check_worda(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a']);
}

bool check_wordab(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a']);
}

bool check_wordabc(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a'])
      && check_letter(s, word[2], letter_mapping[candidate.word[2] - 'a']);
}

bool check_wordabcc(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a'])
      && check_letter(s, word[2], letter_mapping[candidate.word[2] - 'a']);
}

bool check_wordabcd(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a'])
      && check_letter(s, word[2], letter_mapping[candidate.word[2] - 'a'])
      && check_letter(s, word[3], letter_mapping[candidate.word[3] - 'a']);
}

bool check_wordabccd(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a'])
      && check_letter(s, word[2], letter_mapping[candidate.word[2] - 'a'])
      && check_letter(s, word[4], letter_mapping[candidate.word[4] - 'a']);
}

bool check_wordabcde(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a'])
      && check_letter(s, word[2], letter_mapping[candidate.word[2] - 'a'])
      && check_letter(s, word[3], letter_mapping[candidate.word[3] - 'a'])
      && check_letter(s, word[4], letter_mapping[candidate.word[4] - 'a']);
}

bool check_wordabcdef(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a'])
      && check_letter(s, word[2], letter_mapping[candidate.word[2] - 'a'])
      && check_letter(s, word[3], letter_mapping[candidate.word[3] - 'a'])
      && check_letter(s, word[4], letter_mapping[candidate.word[4] - 'a'])
      && check_letter(s, word[5], letter_mapping[candidate.word[5] - 'a']);
}

bool check_wordabcdefgh(const state& s, const word_t& candidate, const char* word) {
  return check_letter(s, word[0], letter_mapping[candidate.word[0] - 'a'])
      && check_letter(s, word[1], letter_mapping[candidate.word[1] - 'a'])
      && check_letter(s, word[2], letter_mapping[candidate.word[2] - 'a'])
      && check_letter(s, word[3], letter_mapping[candidate.word[3] - 'a'])
      && check_letter(s, word[4], letter_mapping[candidate.word[4] - 'a'])
      && check_letter(s, word[5], letter_mapping[candidate.word[5] - 'a'])
      && check_letter(s, word[6], letter_mapping[candidate.word[6] - 'a'])
      && check_letter(s, word[7], letter_mapping[candidate.word[7] - 'a']);
}

const word_t words[NUM_WORDS] = {
  {"a",        patterns[0], &check_worda },
  {"in",       patterns[1], &check_wordab },
  {"of",       patterns[1], &check_wordab },
  {"on",       patterns[1], &check_wordab },
  {"to",       patterns[1], &check_wordab },
  {"up",       patterns[1], &check_wordab },
  {"and",      patterns[2], &check_wordabc },
  {"cow",      patterns[2], &check_wordabc },
  {"dog",      patterns[2], &check_wordabc },
  {"fox",      patterns[2], &check_wordabc },
  {"his",      patterns[2], &check_wordabc },
  {"how",      patterns[2], &check_wordabc },
  {"now",      patterns[2], &check_wordabc },
  {"ran",      patterns[2], &check_wordabc },
  {"the",      patterns[2], &check_wordabc },
  {"came",     patterns[4], &check_wordabcd },
  {"down",     patterns[4], &check_wordabcd },
  {"fell",     patterns[3], &check_wordabcc },
  {"hill",     patterns[3], &check_wordabcc },
  {"jack",     patterns[4], &check_wordabcd },
  {"jill",     patterns[3], &check_wordabcc },
  {"lazy",     patterns[4], &check_wordabcd },
  {"over",     patterns[4], &check_wordabcd },
  {"pail",     patterns[4], &check_wordabcd },
  {"rain",     patterns[4], &check_wordabcd },
  {"after",    patterns[6], &check_wordabcde },
  {"broke",    patterns[6], &check_wordabcde },
  {"brown",    patterns[6], &check_wordabcde },
  {"crown",    patterns[6], &check_wordabcde },
  {"falls",    patterns[5], &check_wordabccd },
  {"fetch",    patterns[6], &check_wordabcde },
  {"jumps",    patterns[6], &check_wordabcde },
  {"quick",    patterns[6], &check_wordabcde },
  {"spain",    patterns[6], &check_wordabcde },
  {"water",    patterns[6], &check_wordabcde },
  {"mainly",   patterns[7], &check_wordabcdef },
  {"plains",   patterns[7], &check_wordabcdef },
  {"tumbling", patterns[8], &check_wordabcdefgh }
};

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
