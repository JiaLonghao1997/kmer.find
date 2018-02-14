from itertools import chain
from skbio import alignment
from fasta_reader import IndexedFastaReader
from skbio import sequence
import skbio.io
from sys import argv
import subprocess

query_fname = argv[1]
index_fname= argv[2]
headers_fname = argv[3]


class GetNames(object):
    def __init__(self, fname):
        self.headers = open(fname, 'rb')
    def get(self, ix):
        self.headers.seek(32 * ix)
        return self.headers.readline().decode('ascii').strip()

Qs = {}
for fa in skbio.io.read(query_fname, format='fasta'):
    Qs[fa.metadata['id']] = str(fa)


headers = GetNames(headers_fname)
print("Loaded headers")

index = IndexedFastaReader(index_fname)

data = subprocess.check_output(['Query', '-i', query_fname, '-o', '/dev/stdout', '-1', 'kmer.index/GMGC.95nr.faa.kmer.ix1', '-2', 'kmer.index/GMGC.95nr.faa.kmer.ix2']).decode('ascii')

matches = []
for line in chain(data.splitlines(), ['END']):
    if line.startswith('CmdArgs'): continue
    if line[0] == '>' or line == 'END':
        if len(matches):
            matches.sort(key=lambda m: m[1]['optimal_alignment_score'], reverse=True)
            for fah, m in matches:
                print(f'{active}\t{fah}\t{m.optimal_alignment_score}')
            matches = []
        if line == "END":
            break
        active = line[1:].strip()
        fa = Qs[active]
        sw = skbio.alignment.StripedSmithWaterman(fa)
    else:
        ix = int(line.strip())
        name = headers.get(ix)
        matches.append((name, sw(index.get(name).decode('ascii'))))

