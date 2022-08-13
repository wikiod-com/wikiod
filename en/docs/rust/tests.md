---
title: "Tests"
slug: "tests"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Test a function
    fn to_test(output: bool) -> bool {
        output
    }

    #[cfg(test)] // The module is only compiled when testing.
    mod test {
        use super::to_test;

        // This function is a test function. It will be executed and
        // the test will succeed if the function exits cleanly.
        #[test]
        fn test_to_test_ok() {
            assert_eq!(to_test(true), true);
        }

        // That test on the other hand will only succeed when the function
        // panics.
        #[test]
        #[should_panic]
        fn test_to_test_fail() {
            assert_eq!(to_test(true), false);
        }
    }

([Playground link](https://is.gd/RSh8jU))

Run with `cargo test`.


## Benchmark tests


With benchmark tests you can test and measure the speed of the code, however benchmark tests are still unstable. To enable benchmarks in your cargo project you need nightly rust, put your integration benchmark tests to the `benches/` folder in the root of your Cargo project, and run `cargo bench`.

Examples from [llogiq.github.io][1]

    extern crate test;
    extern crate rand;
    
    use test::Bencher;
    use rand::Rng;
    use std::mem::replace;
    
    #[bench]
    fn empty(b: &mut Bencher) {
        b.iter(|| 1)
    }
    
    #[bench]
    fn setup_random_hashmap(b: &mut Bencher) {
        let mut val : u32 = 0;
        let mut rng = rand::IsaacRng::new_unseeded();
        let mut map = std::collections::HashMap::new();
    
        b.iter(|| { map.insert(rng.gen::<u8>() as usize, val); val += 1; })
    }
    
    #[bench]
    fn setup_random_vecmap(b: &mut Bencher) {
        let mut val : u32 = 0;
        let mut rng = rand::IsaacRng::new_unseeded();
        let mut map = std::collections::VecMap::new();
    
        b.iter(|| { map.insert((rng.gen::<u8>()) as usize, val); val += 1; })
    }
    
    #[bench]
    fn setup_random_vecmap_cap(b: &mut Bencher) {
        let mut val : u32 = 0;
        let mut rng = rand::IsaacRng::new_unseeded();
        let mut map = std::collections::VecMap::with_capacity(256);
    
        b.iter(|| { map.insert((rng.gen::<u8>()) as usize, val); val += 1; })
    }

  [1]: https://llogiq.github.io/2015/06/16/bench.html "Benchmarking in Rust."


## Integration Tests
`lib.rs`:

    pub fn to_test(output: bool) -> bool {
        output
    }

Each file in the `tests/` folder is compiled as single crate.
`tests/integration_test.rs`

    extern crate test_lib;
    use test_lib::to_test;
    
    #[test]
    fn test_to_test(){
        assert_eq!(to_test(true), true);
    }

