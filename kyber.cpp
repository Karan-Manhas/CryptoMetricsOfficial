#include <iostream>
#include <vector>
#include <chrono>
#include <cstring> 
#include <fstream> 
#include <sstream>
#include <iomanip>
#include <ctime>
#include <sys/time.h>
#include <unistd.h> 
#include <malloc.h> 
#include "oqs/oqs.h"

std::string get_filename_with_timestamp() {
    auto now = std::chrono::system_clock::now();
    auto now_c = std::chrono::system_clock::to_time_t(now);

    std::stringstream ss;
    ss << "kyber_benchmark_"
       << std::put_time(std::localtime(&now_c), "%Y%m%d_%H%M%S")
       << ".csv";

    return ss.str();
}
inline int64_t get_unix_timestamp_ns() {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::system_clock::now().time_since_epoch()).count();
}

double get_cpu_usage(clock_t start_clock) {
    clock_t end_clock = clock();
    double cpu_time_used = ((double) (end_clock - start_clock)) / CLOCKS_PER_SEC;
    return cpu_time_used * 100.0; 
}

void benchmark_kem(const char *kem_alg, int num_iterations) {
    std::string filename = get_filename_with_timestamp();

    std::ofstream csv_file(filename);
    if (!csv_file) {
        std::cerr << "Error opening CSV file for writing\n";
        return;
    }

    //  CSV header
    csv_file << "algorithm_name,iteration_no,crypto_operation,start_timestamp,end_timestamp,time_ns,key_size_bytes,ciphertext_size_bytes,shared_secret_key_size_bytes,cpu_percent\n";

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

        // keygen
        clock_t keygen_start_clock = clock();
        int64_t keygen_start_ns = get_unix_timestamp_ns();
        OQS_KEM_keypair(kem, public_key.data(), secret_key.data());
        int64_t keygen_end_ns = get_unix_timestamp_ns();
        double keygen_cpu_percent = get_cpu_usage(keygen_start_clock);
        int64_t keygen_duration = keygen_end_ns - keygen_start_ns;

        //encaps
        clock_t encaps_start_clock = clock();
        int64_t encaps_start_ns = get_unix_timestamp_ns();
        OQS_KEM_encaps(kem, ciphertext.data(), shared_secret_enc.data(), public_key.data());
        int64_t encaps_end_ns = get_unix_timestamp_ns();
        double encaps_cpu_percent = get_cpu_usage(encaps_start_clock);
        int64_t encaps_duration = encaps_end_ns - encaps_start_ns;

          //decaps
        clock_t decaps_start_clock = clock();
        int64_t decaps_start_ns = get_unix_timestamp_ns();
        OQS_KEM_decaps(kem, shared_secret_dec.data(), ciphertext.data(), secret_key.data());
        int64_t decaps_end_ns = get_unix_timestamp_ns();
        double decaps_cpu_percent = get_cpu_usage(decaps_start_clock);
        int64_t decaps_duration = decaps_end_ns - decaps_start_ns;

        //validation
        bool success = (memcmp(shared_secret_enc.data(), shared_secret_dec.data(), kem->length_shared_secret) == 0);
        if (!success) {
            std::cerr << "Error: Shared secrets do NOT match!\n";
        }

        // results to CSV file
        csv_file << kem_alg << "," << i << ",Key Generation,"
                 << keygen_start_ns << "," << keygen_end_ns << ","
                 << keygen_duration << "," << kem->length_public_key << ",0,0," << keygen_cpu_percent << "\n";

        csv_file << kem_alg << "," << i << ",Encapsulation,"
                 << encaps_start_ns << "," << encaps_end_ns << ","
                 << encaps_duration << "," << 0 << "," << kem->length_ciphertext << ","
                 << kem->length_shared_secret << "," << encaps_cpu_percent << "\n";

        csv_file << kem_alg << "," << i << ",Decapsulation,"
                 << decaps_start_ns << "," << decaps_end_ns << ","
                 << decaps_duration << "," << 0 << ",0," << kem->length_shared_secret << ","
                 << decaps_cpu_percent << "\n";

        // cleanup
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

    std::cout << "Starting Kyber Benchmark with " << num_iterations << " iterations...\n";
    benchmark_kem(kem_alg, num_iterations);
    return 0;
}
