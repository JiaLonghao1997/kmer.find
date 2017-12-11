from skbio import alignment
from fasta_reader import IndexedFastaReader
from skbio import sequence
import skbio.io
from sys import argv

query_fname = argv[1]
index_fname= argv[2]
headers_fname = argv[3]

Qs = {}
for fa in skbio.io.read(query_fname, format='fasta'):
    Qs[fa.metadata['id']] = str(fa)


headers = [line.strip() for line in open(headers_fname)]
index = IndexedFastaReader(index_fname)

matches = []
for line in open('re.output.txt'):
    if line.startswith('CmdArgs'): continue
    if line[0] == '>':
        if len(matches):
            matches.sort(key=lambda m: m[1]['optimal_alignment_score'], reverse=True)
            for fah, m in matches:
                print(f'{active}\t{fah}\t{m.optimal_alignment_score}')
            matches = []
        active = line[1:].strip()
        fa = Qs[active]
        sw = skbio.alignment.StripedSmithWaterman(fa)
    else:
        ix = int(line.strip())
        matches.append((headers[ix], sw(index.get(headers[ix]).decode('ascii'))))
