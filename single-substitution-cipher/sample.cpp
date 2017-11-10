#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <vector>

#define NUM_LETTERS 26
#define NUM_WORDS 38
#define NUM_PATTERNS 9
#define MAX_PATTERN_SIZE 8


const char* const cipherText = \
    "The rain in spain falls mainly on the plains\n" \
    "The quick brown fox jumps over the lazy dog\n" \
    "How now brown cow\n" \
    "Jack and Jill ran up the hill to fetch a pail of water\n" \
    "Jack fell down and broke his crown and Jill came tumbling after\n";

typedef struct patternRange {
    int from;
    int to;
} patternRange;

const char* const letters = "anoilerwcfthpsmubdjkgyqvxz";

const char* const patterns[NUM_PATTERNS] = {"a", "ab", "abc", "abcc", "abcd", "abccd", "abcde", "abcdef", "abcdefgh"};
const patternRange patternIndex[MAX_PATTERN_SIZE + 1] = {{-1,-1}, {0,0}, {1,1}, {2,2}, {3,4}, {5,6}, {7,7}, {-1,-1}, {8,8}};

typedef std::vector<const char*> wordList;
typedef std::map<const char*, wordList*> wordMap;

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

typedef bool letterToggle_t[NUM_LETTERS];
typedef bool wordToggle_t[NUM_WORDS];

typedef struct letterMapping {
  int wordIndex;
  int encryptedIndex;
} letterMapping_t;

typedef struct word {
  const char* const word;
  const char* const pattern;
  int numMappings;
  // Boring hack for now, while I suss this out since the compiler wouldn't recursively
  // generate this for me.
  const letterMapping_t mappings[8];
} word_t;

#define NUM_WORDS 38
const word_t words[NUM_WORDS] = {
  {"a", patterns[0], 1, {{0, 0}}},
  {"in", patterns[1], 2, {{0, 3}, {1, 1}}},
  {"of", patterns[1], 2, {{0, 2}, {1, 9}}},
  {"on", patterns[1], 2, {{0, 2}, {1, 1}}},
  {"to", patterns[1], 2, {{0, 10}, {1, 2}}},
  {"up", patterns[1], 2, {{0, 15}, {1, 12}}},
  {"and", patterns[2], 3, {{0, 0}, {1, 1}, {2, 17}}},
  {"cow", patterns[2], 3, {{0, 8}, {1, 2}, {2, 7}}},
  {"dog", patterns[2], 3, {{0, 17}, {1, 2}, {2, 20}}},
  {"fox", patterns[2], 3, {{0, 9}, {1, 2}, {2, 24}}},
  {"his", patterns[2], 3, {{0, 11}, {1, 3}, {2, 13}}},
  {"how", patterns[2], 3, {{0, 11}, {1, 2}, {2, 7}}},
  {"now", patterns[2], 3, {{0, 1}, {1, 2}, {2, 7}}},
  {"ran", patterns[2], 3, {{0, 6}, {1, 0}, {2, 1}}},
  {"the", patterns[2], 3, {{0, 10}, {1, 11}, {2, 5}}},
  {"came", patterns[4], 4, {{0, 8}, {1, 0}, {2, 14}, {3, 5}}},
  {"down", patterns[4], 4, {{0, 17}, {1, 2}, {2, 7}, {3, 1}}},
  {"fell", patterns[3], 3, {{0, 9}, {1, 5}, {2, 4}}},
  {"hill", patterns[3], 3, {{0, 11}, {1, 3}, {2, 4}}},
  {"jack", patterns[4], 4, {{0, 18}, {1, 0}, {2, 8}, {3, 19}}},
  {"jill", patterns[3], 3, {{0, 18}, {1, 3}, {2, 4}}},
  {"lazy", patterns[4], 4, {{0, 4}, {1, 0}, {2, 25}, {3, 21}}},
  {"over", patterns[4], 4, {{0, 2}, {1, 23}, {2, 5}, {3, 6}}},
  {"pail", patterns[4], 4, {{0, 12}, {1, 0}, {2, 3}, {3, 4}}},
  {"rain", patterns[4], 4, {{0, 6}, {1, 0}, {2, 3}, {3, 1}}},
  {"after", patterns[6], 5, {{0, 0}, {1, 9}, {2, 10}, {3, 5}, {4, 6}}},
  {"broke", patterns[6], 5, {{0, 16}, {1, 6}, {2, 2}, {3, 19}, {4, 5}}},
  {"brown", patterns[6], 5, {{0, 16}, {1, 6}, {2, 2}, {3, 7}, {4, 1}}},
  {"crown", patterns[6], 5, {{0, 8}, {1, 6}, {2, 2}, {3, 7}, {4, 1}}},
  {"falls", patterns[5], 4, {{0, 9}, {1, 0}, {2, 4}, {4, 13}}},
  {"fetch", patterns[6], 5, {{0, 9}, {1, 5}, {2, 10}, {3, 8}, {4, 11}}},
  {"jumps", patterns[6], 5, {{0, 18}, {1, 15}, {2, 14}, {3, 12}, {4, 13}}},
  {"quick", patterns[6], 5, {{0, 22}, {1, 15}, {2, 3}, {3, 8}, {4, 19}}},
  {"spain", patterns[6], 5, {{0, 13}, {1, 12}, {2, 0}, {3, 3}, {4, 1}}},
  {"water", patterns[6], 5, {{0, 7}, {1, 0}, {2, 10}, {3, 5}, {4, 6}}},
  {"mainly", patterns[7], 6, {{0, 14}, {1, 0}, {2, 3}, {3, 1}, {4, 4}, {5, 21}}},
  {"plains", patterns[7], 6, {{0, 12}, {1, 4}, {2, 0}, {3, 3}, {4, 1}, {5, 13}}},
  {"tumbling", patterns[8], 8, {{0, 10}, {1, 15}, {2, 14}, {3, 16}, {4, 4}, {5, 3}, {6, 1}, {7, 20}}}
};

typedef struct inclusion {
  int numWords;
  // Hack for now.
  int words[NUM_WORDS];
} inclusion_t;

const inclusion_t inclusions[NUM_LETTERS] = {{14, {0, 6, 13, 15, 19, 21, 23, 24, 25, 29, 33, 34, 35, 36}}, {13, {1, 3, 6, 12, 13, 16, 24, 27, 28, 33, 35, 36, 37}}, {13, {2, 3, 4, 7, 8, 9, 11, 12, 16, 22, 26, 27, 28}}, {11, {1, 10, 18, 20, 23, 24, 32, 33, 35, 36, 37}}, {9, {17, 18, 20, 21, 23, 29, 35, 36, 37}}, {8, {14, 15, 17, 22, 25, 26, 30, 34}}, {8, {13, 22, 24, 25, 26, 27, 28, 34}}, {7, {7, 11, 12, 16, 27, 28, 34}}, {6, {7, 15, 19, 28, 30, 32}}, {6, {2, 9, 17, 25, 29, 30}}, {6, {4, 14, 25, 30, 34, 37}}, {5, {10, 11, 14, 18, 30}}, {5, {5, 23, 31, 33, 36}}, {5, {10, 29, 31, 33, 36}}, {4, {15, 31, 35, 37}}, {4, {5, 31, 32, 37}}, {3, {26, 27, 37}}, {3, {6, 8, 16}}, {3, {19, 20, 31}}, {3, {19, 26, 32}}, {2, {8, 37}}, {2, {21, 35}}, {1, {32}}, {1, {22}}, {1, {9}}, {1, {21}}};

wordMap wordsByPattern;

void makePattern(const char* buffer, char* pattern) {
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

void buildWordList(const char* const filename) {
  FILE* input = fopen(filename, "r");
  char buffer[128];
  char pattern[128];
  int len, i;
  patternRange range;
  const char * cloned;
  if (!input) {
    fprintf(stderr, "Failed to open word file list");
    exit(1);
  }
  for (i = 0; i < NUM_PATTERNS; ++i) {
    wordsByPattern.insert(std::make_pair(patterns[i], new wordList));
  }
  while(!feof(input)) {
    if (!fgets(buffer, 128, input)) {
      break;
    }
    len = strlen(buffer) - 1;
    buffer[len] = '\0';
    if (len > MAX_PATTERN_SIZE || patternIndex[len].from == -1) {
      continue;
    }
    makePattern(buffer, pattern);
    range = patternIndex[len];
    for(i = range.from; i <= range.to; ++i) {
      if (strcmp(pattern, patterns[i]) == 0) {
        goto found;
      }
    }
    continue;
  found:
    cloned = strdup(buffer);
    wordsByPattern[patterns[i]]->push_back(cloned);
  }
  fclose(input);
}

bool checkWord(const state& s, const word_t& candidate, const char* word) {
  int i, j, numMappings, numPossible, ci;
  char c;
  numMappings = candidate.numMappings;
  for (i = 0; i < numMappings; ++i) {
    c = word[candidate.mappings[i].wordIndex];
    ci = candidate.mappings[i].encryptedIndex;
    numPossible = s.characters[ci].numPossible;
    for (j = 0; j < numPossible; ++j) {
      if (s.characters[ci].possibilities[j] == c) {
        goto found;
      }
    }
    return false;
  found:
    ;
  }
  return true;
}

void filterWords(const state& s, const word_t& candidate, const wordList& initial, wordList& final) {
  wordList::const_iterator it;
  for (it = initial.begin(); it != initial.end(); ++it) {
    if (checkWord(s, candidate, *it)) {
      final.push_back(*it);
    }
  }
}

bool makeUnique(state& s, const int ci, letterToggle_t &changed) {
  char c = s.characters[ci].possibilities[0];
  for(int i = 0; i < NUM_LETTERS; ++i) {
    if (i == ci) {
      continue;
    }
    charPossibilities& cp = s.characters[i];
    for (int j = 0; j < cp.numPossible; ++j) {
      if (cp.possibilities[j] == c) {
        if (cp.numPossible == 1) {
          return false;
        } else if (j != cp.numPossible - 1) {
          memmove(cp.possibilities + j, cp.possibilities + j + 1, cp.numPossible - j - 1);
        }
        cp.numPossible -= 1;
        changed[i] = true;
        if (cp.numPossible == 1) {
          if (!makeUnique(s, i, changed)) {
            return false;
          }
        }
      }
    }
  }
  return true;
}

bool constrainLetters(state& s, const int wi, letterToggle_t &changed) {
  const word_t& word = words[wi];
  const wordList* ws = s.words[wi].words;

  bool seen[NUM_LETTERS][26] = {0};
  bool used[NUM_LETTERS] = {0};

  for (wordList::const_iterator it = ws->begin(); it != ws->end(); ++it) {
    for (int i = 0; i < word.numMappings; ++i) {
      seen[word.mappings[i].encryptedIndex][(*it)[word.mappings[i].wordIndex] - 'a'] = true;
      used[word.mappings[i].encryptedIndex] = true;
    }
  }

  for (int i = 0; i < NUM_LETTERS; ++i) {
    if (used[i]) {
      charPossibilities &cp = s.characters[i];
      bool shrunk = false;
      char *writeHead;
      for (int j = 0; j < cp.numPossible; ++j) {
        if (seen[i][cp.possibilities[j] - 'a']) {
          if (shrunk) {
            *writeHead++ = cp.possibilities[j];
          }
        } else if (!shrunk) {
          shrunk = true;
          writeHead = cp.possibilities + j;
        }
      }
      if (shrunk) {
        cp.numPossible = writeHead - cp.possibilities;
        changed[i] = true;
        if (cp.numPossible == 0) {
          return false;
        } else if (cp.numPossible == 1) {
          if (!makeUnique(s, i, changed)) {
            return false;
          }
        }
      }
    }
  }

  return true;
}

bool checkConstraints(state& s, letterToggle_t& changed) {
  wordToggle_t wordsChanged = {0};
  letterToggle_t followUp = {0};
  bool anyChanged = false;
  for (int i = 0; i < NUM_LETTERS; ++i) {
    if (changed[i]) {
      anyChanged = true;
      for(int j = 0; j < inclusions[i].numWords; ++j) {
        wordsChanged[inclusions[i].words[j]] = true;
      }
    }
  }
  if (!anyChanged) {
    return true;
  }
  for (int i = 0; i < NUM_WORDS; ++i) {
    if (wordsChanged[i]) {
      wordList* newWords = new wordList;
      filterWords(s, words[i], *s.words[i].words, *newWords);
      if (newWords->size() == 0) {
        delete newWords;
        return false;
      } else if (newWords->size() == s.words[i].words->size()) {
        delete newWords;
      } else {
        if (s.words[i].isUsingRoot) {
          s.words[i].isUsingRoot = false;
        } else {
          delete s.words[i].words;
        }
        s.words[i].words = newWords;
        if (!constrainLetters(s, i, followUp)) {
          return false;
        }
      }
    }
  }
  return checkConstraints(s, followUp);
}

int solutionCount = 1;
void guessLetter(const state& s, int ci) {
  if (ci >= NUM_LETTERS) {
    printf("=== Found Solution %d ===\n", solutionCount++);
    for (int i = 0; i < NUM_LETTERS; ++i) {
      printf("%c", s.characters[i].c);
      if (s.characters[i].numPossible != 1) {
        printf(": invalid state!\n");
      }
    }
    printf("\n");
    for (int i = 0; i < NUM_LETTERS; ++i) {
      printf("%c", s.characters[i].possibilities[0]);
    }
    printf("\n");
    for (int i = 0; i < NUM_WORDS; ++i) {
      printf("%s: ", words[i].word);
      if (s.words[i].words->size() == 1) {
        printf("%s\n", *(s.words[i].words->begin()));
      } else {
        printf("*Invalid, %d words*\n", (int)s.words[i].words->size());
      }
    }
    printf("=== ---------------- ===\n");
  } else {
    if (s.characters[ci].numPossible == 1) {
      return guessLetter(s, ci + 1);
    }
    letterToggle_t changed;
    for (int i = 0; i < s.characters[ci].numPossible; ++i) {
      state ns = s;
      memset(changed, 0, sizeof(changed));
      changed[ci] = true;
      ns.characters[ci].possibilities[0] = ns.characters[ci].possibilities[i];
      ns.characters[ci].numPossible = 1;
      for(int wi = 0; wi < NUM_WORDS; ++ wi) {
        ns.words[wi].isUsingRoot = true;
      }
      if (makeUnique(ns, ci, changed) && checkConstraints(ns, changed)) {
        guessLetter(ns, ci + 1);
      }
      for(int wi = 0; wi < NUM_WORDS; ++wi) {
        if (!ns.words[wi].isUsingRoot) {
          delete ns.words[wi].words;
          ns.words[wi].isUsingRoot = true;
          ns.words[wi].words = NULL;
        }
      }
    }
  }
}

int main(int argc, const char* const argv[]) {
  state s;
  buildWordList("words.txt");
  for (int i = 0; i < NUM_LETTERS; ++i) {
    s.characters[i].c = letters[i];
    s.characters[i].numPossible = 26;
    for (char c = 'a'; c <= 'z'; ++c) {
      s.characters[i].possibilities[c - 'a'] = c;
    }
  }
  for (int i = 0; i < NUM_WORDS; ++i) {
    s.words[i].isUsingRoot = true;
    s.words[i].words = wordsByPattern[words[i].pattern];
  }
  guessLetter(s, 0);
  for (wordMap::iterator pit = wordsByPattern.begin(); pit != wordsByPattern.end(); ++pit) {
    for (wordList::iterator it = pit->second->begin(); it != pit->second->end(); ++it) {
      free((void*)*it);
    }
    pit->second->clear();
    delete pit->second;
  }
  wordsByPattern.clear();
  return 0;
}
