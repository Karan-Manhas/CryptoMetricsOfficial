#include "openssl/ec.h"
#include "openssl/ecdh.h"
#include "openssl/evp.h"
#include "openssl/err.h"
#include <iostream>
#include <vector>
#include <chrono>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <ctime>

std::string get_filename_with_timestamp() {
    auto now = std::chrono::system_clock::now();
    auto now_c = std::chrono::system_clock::to_time_t(now);

    std::stringstream ss;
    ss << "ecdh_benchmark_"
       << std::put_time(std::localtime(&now_c), "%Y%m%d_%H%M%S")
       << ".csv";

    return ss.str();
}

inline int64_t get_unix_timestamp_ns() {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::system_clock::now().time_since_epoch()).count();
}

//  calculate cpu usage as %
double get_cpu_usage(clock_t start_clock) {
    clock_t end_clock = clock();
    double cpu_time_used = ((double) (end_clock - start_clock)) / CLOCKS_PER_SEC;
    return cpu_time_used * 100.0;
}

// generate new EC key pair
EC_KEY* generate_ec_key() {
    EC_KEY *key = EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);  // P-256 curve
    if (!key || !EC_KEY_generate_key(key)) {
        std::cerr << "Error generating EC key\n";
        return nullptr;
    }
    return key;
}

// compute the shared secret
std::vector<unsigned char> compute_shared_secret(EC_KEY* key, const EC_KEY* peer_key) {
    size_t field_size = (EC_GROUP_get_degree(EC_KEY_get0_group(key)) + 7) / 8;
    std::vector<unsigned char> shared_secret(field_size);

    const EC_POINT* peer_public = EC_KEY_get0_public_key(peer_key);
    int secret_len = ECDH_compute_key(shared_secret.data(), shared_secret.size(), peer_public, key, nullptr);
    if (secret_len <= 0) {
        std::cerr << "Error computing shared secret\n";
        return {};
    }

    shared_secret.resize(secret_len);
    return shared_secret;
}

void benchmark_ecdh(int num_iterations) {
    std::string filename = get_filename_with_timestamp();
    std::ofstream csv_file(filename);
    if (!csv_file) {
        std::cerr << "Error opening CSV file for writing\n";
        return;
    }
    csv_file << "algorithm_name,iteration_no,crypto_operation,start_timestamp,end_timestamp,time_ns,key_size_bytes,shared_secret_key_size_bytes,cpu_percent\n";

    for (int i = 1; i <= num_iterations; i++) {
        std::cout << "Running iteration " << i << "..." << std::endl;

        clock_t keygen_start_clock = clock();
        int64_t keygen_start_ns = get_unix_timestamp_ns();
        EC_KEY* alice_key = generate_ec_key();
        EC_KEY* bob_key = generate_ec_key();
        int64_t keygen_end_ns = get_unix_timestamp_ns();
        double keygen_cpu_percent = get_cpu_usage(keygen_start_clock);
        int64_t keygen_duration = keygen_end_ns - keygen_start_ns;

        if (!alice_key || !bob_key) {
            std::cerr << "Failed to generate keys\n";
            return;
        }

        clock_t exchange_start_clock = clock();
        int64_t exchange_start_ns = get_unix_timestamp_ns();
        std::vector<unsigned char> alice_shared_secret = compute_shared_secret(alice_key, bob_key);
        std::vector<unsigned char> bob_shared_secret = compute_shared_secret(bob_key, alice_key);
        int64_t exchange_end_ns = get_unix_timestamp_ns();
        double exchange_cpu_percent = get_cpu_usage(exchange_start_clock);
        int64_t exchange_duration = exchange_end_ns - exchange_start_ns;

        if (alice_shared_secret != bob_shared_secret) {
            std::cerr << "Shared secrets do not match!\n";
        }

        // write to CSV
        csv_file << "ECDH," << i << ",Key Generation,"
                 << keygen_start_ns << "," << keygen_end_ns << ","
                 << keygen_duration << "," << EC_GROUP_get_degree(EC_KEY_get0_group(alice_key)) / 8 << ",0," << keygen_cpu_percent << "\n";

        csv_file << "ECDH," << i << ",Key Exchange,"
                 << exchange_start_ns << "," << exchange_end_ns << ","
                 << exchange_duration << ",0," << alice_shared_secret.size() << "," << exchange_cpu_percent << "\n";

        // cleanup
        EC_KEY_free(alice_key);
        EC_KEY_free(bob_key);
    }

    csv_file.close();
    std::cout << "Benchmark results saved to: " << filename << std::endl;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <number_of_iterations>\n";
        return 1;
    }

    int num_iterations = std::stoi(argv[1]);
    if (num_iterations <= 0) {
        std::cerr << "Error: Number of iterations must be positive.\n";
        return 1;
    }

    std::cout << "Starting ECDH Benchmark with " << num_iterations << " iterations...\n";
    benchmark_ecdh(num_iterations);
    return 0;
}
