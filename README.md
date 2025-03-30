# CryptoMetrics | Ubuntu Edition 
- A repo dedicated for evaluating the performance of CRYSTAL - Kyber and ECDH based on execution_time in ns, cpu utilisation and memory usage.
## Relevant files/folders
Below describes the relevant cpp files for benchmarking.
### Relevant`.cpp`files 
- `kyber.cpp`
  - contains kyber implementation, specifically ML-KEM 1024 (time and cpu benchmarking).
- `ecdh.cpp`
  - contains ecdh implementation, based on the P-256 curve (time and cpu benchmarking).
- `kyber_ram.cpp`
  - contains kyber implementation, specifically ML-KEM 1024 (Memory benchmarking).
- `ecdh_ram.cpp`
  - contains ecdh implementation, based on the P-256 curve (Memory Benchmarking).

### Relevant Libraries 
- `external` folder
- For external dependencies required
    - Contains **OpenSSL** library : `(openssl-3.4.0)` validated by `git log --oneline -1` found in `commit: 98acb6b02839c609ef5b837794e08d906d965335`
    - Contains **LibOQS** standard`(Import ML-KEM from mlkem-native/PQ code package (#2041) found in `commit: a554b36dd321e94c276e85c025f350c70740f328`

## Executing the executables
- Due to the configuration set up, once the project is built, it is easy to execute and generate csv files required.

- Navigate to build via cl 
  - `./ecdh 1000`
  - `./ecdh_ram 1000`

- Navigate to build via cl
  - `./kyber Kyber1024 1000`
  - `./kyber_ram Kyber1024 1000`

## Test Conditions
- Using command `who -b` the ec2 was started at : `system boot  2025-02-14 11:35`
- To ensure fair testing conditions, the EC2 was allocated some uptime prior to benchmarking test.
	- `uptime -p` was used prior to benchmarking which returned an uptime of `12 minutes`
	- `uptime -p` was used after benchmarking with return an uptime of `15 minutes`

## Raw Data Output
- All Raw Data files in this project can be found in the `.csv` files within this repo#   C r y p t o M e t r i c s O f f i c i a l  
 