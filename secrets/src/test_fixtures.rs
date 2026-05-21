//! Hardcoded ed25519 SSH keypairs for crate-internal tests.
//!
//! Three keypairs are enough for every fixture in the suite (duplicate-key
//! checks, host vs target identities, multi-machine setups). Generated once
//! via `ssh-keygen -t ed25519 -N "" -f /tmp/k` and pasted here verbatim -
//! these are test material with no security purpose.

#![cfg(test)]

pub(crate) const TEST_SSH_ED25519_A_PRIV: &str = "-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACB7Ci6nqZYaVvrjm8+XbzII89TsXzP111AflR7WeorBjQAAAJCfEwtqnxML
agAAAAtzc2gtZWQyNTUxOQAAACB7Ci6nqZYaVvrjm8+XbzII89TsXzP111AflR7WeorBjQ
AAAEADBJvjZT8X6JRJI8xVq/1aU8nMVgOtVnmdwqWwrSlXG3sKLqeplhpW+uObz5dvMgjz
1OxfM/XXUB+VHtZ6isGNAAAADHN0cjRkQGNhcmJvbgE=
-----END OPENSSH PRIVATE KEY-----
";

pub(crate) const TEST_SSH_ED25519_A_PUB: &str =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN str4d@carbon";

pub(crate) const TEST_SSH_ED25519_B_PRIV: &str = "-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACCcv4xyOKvlPfRWMTZRBtJqXJ7t9RfgxqtsrpSwzI/KmAAAAJDI9gF0yPYB
dAAAAAtzc2gtZWQyNTUxOQAAACCcv4xyOKvlPfRWMTZRBtJqXJ7t9RfgxqtsrpSwzI/KmA
AAAEDXnF3ppY4eJ9cEW/DhJAi+XPRa+ZNKHse5nGOhOOdkO5y/jHI4q+U99FYxNlEG0mpc
nu31F+DGq2yulLDMj8qYAAAACWZpeHR1cmVfYgECAwQ=
-----END OPENSSH PRIVATE KEY-----
";

pub(crate) const TEST_SSH_ED25519_B_PUB: &str =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJy/jHI4q+U99FYxNlEG0mpcnu31F+DGq2yulLDMj8qY fixture_b";

pub(crate) const TEST_SSH_ED25519_C_PUB: &str =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPOns7rMXpxkp2GZXeLN4n19Il30m8vrmVD7Aa2U69ub fixture_c";
