#include <iostream>
#include <vector>
#include <chrono>
#include <cstring>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <ctime>
#include <malloc.h>
#include <sys/resource.h>
#include <unistd.h>
#include "oqs/oqs.h"

std::string get_filename_with_timestamp() {
    auto now = std::chrono::system_clock::now();
    auto now_c = std::chrono::system_clock::to_time_t(now);

    std::stringstream ss;
    ss << "kyber_benchmark_ram_"
       << std::put_time(std::localtime(&now_c), "%Y%m%d_%H%M%S")
       << ".csv";

    return ss.str();
}

inline int64_t get_unix_timestamp_ns() {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::system_clock::now().time_since_epoch()).count();
}

//   get the current heap memory usage in kb
size_t get_heap_memory_kb() {
    struct mallinfo2 info = mallinfo2();
    return info.uordblks / 1024;  //bytes to KB
}

// get peak resident set size (RSS) in KB
size_t get_peak_rss_kb() {
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    return usage.ru_maxrss; //resident set size in KB
}

// Function to benchmark Kyber KEM and write results to a CSV file
void benchmark_kem(const char *kem_alg, int num_iterations) {
    std::string filename = get_filename_with_timestamp();
    std::ofstream csv_file(filename);
    if (!csv_file) {
        std::cerr << "Error opening CSV file for writing\n";
        return;
    }

    // write CSV header
    csv_file << "algorithm_name,iteration_no,crypto_operation,start_timestamp,end_timestamp,key_size_bytes,ciphertext_size_bytes,shared_secret_key_size_bytes,heap_memory_kb,peak_rss_kb\n";

    for (int i = 1; i <= num_iterations; i++) {
        std::cout << "Running iteration " << i << "..." << std::endl;

        OQS_KEM *kem = OQS_KEM_new(kem_alg);
        if (kem == nullptr) {
            std::cerr << "Failed to initialize KEM: " << kem_alg << std::endl;
            return;
        }

        std::vector<uint8_t> public_key(kem->length_public_key);
        std::vector<uint8_t> secret_key(kem->length_secret_key);
        std::vector<uint8_t> ciphertext(kem->length_ciphertext);
        std::vector<uint8_t> shared_secret_enc(kem->length_shared_secret);
        std::vector<uint8_t> shared_secret_dec(kem->length_shared_secret);

        // key gen
        int64_t start_ts = get_unix_timestamp_ns();
        size_t heap_memory_kb = get_heap_memory_kb();
        OQS_KEM_keypair(kem, public_key.data(), secret_key.data());
        int64_t end_ts = get_unix_timestamp_ns();
        size_t peak_rss_kb = get_peak_rss_kb();

        csv_file << kem_alg << "," << i << ",Key Generation,"
                 << start_ts << "," << end_ts << ","
                 << kem->length_public_key << ",0,0," 
                 << heap_memory_kb << "," << peak_rss_kb << "\n";

        // encaps
        start_ts = get_unix_timestamp_ns();
        heap_memory_kb = get_heap_memory_kb();
        OQS_KEM_encaps(kem, ciphertext.data(), shared_secret_enc.data(), public_key.data());
        end_ts = get_unix_timestamp_ns();
        peak_rss_kb = get_peak_rss_kb();

        csv_file << kem_alg << "," << i << ",Encapsulation,"
                 << start_ts << "," << end_ts << ","
                 << "0," << kem->length_ciphertext << "," << kem->length_shared_secret << ","
                 << heap_memory_kb << "," << peak_rss_kb << "\n";

        //decaps
        start_ts = get_unix_timestamp_ns();
        heap_memory_kb = get_heap_memory_kb();
        OQS_KEM_decaps(kem, shared_secret_dec.data(), ciphertext.data(), secret_key.data());
        end_ts = get_unix_timestamp_ns();
        peak_rss_kb = get_peak_rss_kb();

        csv_file << kem_alg << "," << i << ",Decapsulation,"
                 << start_ts << "," << end_ts << ","
                 << "0,0," << kem->length_shared_secret << ","
                 << heap_memory_kb << "," << peak_rss_kb << "\n";

        //cleanup
        OQS_KEM_free(kem);
    }

    csv_file.close();
    std::cout << "Benchmark results saved to: " << filename << std::endl;
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <KEM_algorithm> <number_of_iterations>\n";
        return 1;
    }

    const char* kem_alg = argv[1];
    int num_iterations = std::stoi(argv[2]);

    if (num_iterations <= 0) {
        std::cerr << "Error: Number of iterations must be positive.\n";
        return 1;
    }

    std::cout << "Starting Kyber RAM Benchmark with " << num_iterations << " iterations...\n";
    benchmark_kem(kem_alg, num_iterations);
    return 0;
}
