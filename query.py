from os import path
from itertools import chain
from fasta_reader import IndexedFastaReader
import skbio.alignment
import skbio.sequence
import skbio.io
from sys import argv
import subprocess
import math
from BLOSUM import blosum62, blosum50

index_fname = argv[1]
query_fname = argv[2]

index_base, index_fname  = path.split(path.abspath(index_fname))

class GetNames(object):
    def __init__(self, fname):
        self.headers = open(fname, 'rb')
    def get(self, ix):
        self.headers.seek(32 * ix)
        return self.headers.readline().decode('ascii').strip()
        
Qs = {}
for fa in skbio.io.read(query_fname, format='fasta'):
    Qs[fa.metadata['id']] = str(fa)

    
headers = GetNames(f'{index_base}/kmer.index/{index_fname}.names.32')
print("Loaded headers") 
    
index = IndexedFastaReader(path.join(index_base, index_fname))
    
data = subprocess.Popen(['Query',
                '-i', query_fname,
                '-o', '/dev/stdout',
                '-1', f'{index_base}/kmer.index/{index_fname}.kmer.ix1',
                '-2', f'{index_base}/kmer.index/{index_fname}.kmer.ix2'],
        stdout=subprocess.PIPE)

matches = []	
lamda =	 0.318                        # λ is the Gumble distribution constant
K = 0.13                              # K is a constant associated with the scoring matrix used.
database_size = 0
for line in open(f'{index_base}/{index_fname}.databasesize'):
    database_size = database_size + int(line.strip())  #the size of the database(m)

for line in chain(data.stdout, [b'END']):
    if line.startswith(b'CmdArgs'): continue
    line = line.decode('ascii')
    if line[0] == '>' or line == 'END':
        if len(matches):
            matches.sort(key=lambda m: m[1]['optimal_alignment_score'], reverse=True)
            for fah, m in matches:
                bit_score = (lamda * m.optimal_alignment_score - math.log(K)) / math.log(2)	
                p_value = math.pow(2, (-bit_score))
                E_value = database_size * len(m.query_sequence) * p_value
                print(f'{active}\t{fah}\t{m.optimal_alignment_score}\t{bit_score}\t{E_value}')
            matches = []
        if line == "END":
            break
        active = line[1:].strip()
        fa = Qs[active]
        sw = skbio.alignment.StripedSmithWaterman(fa, substitution_matrix=blosum62,gap_open_penalty=11,gap_extend_penalty=1)
    else:
        name = headers.get(int(line.strip()))
        seq = index.get(name).decode('ascii')
        matches.append((name, sw(seq)))
