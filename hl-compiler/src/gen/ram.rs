use std::collections::HashMap;

pub struct Ram {
    variables: HashMap<String, usize>,
    allocated_memory: Vec<bool>
}

impl Ram {
    pub fn new(size: usize) -> Ram {
        let mut allocated_memory = vec![false; size];
        allocated_memory[0] = true;
        Ram {
            variables: HashMap::new(),
            allocated_memory
        }
    }

    pub fn contains(&self, variable: &String) -> bool {
        self.variables.contains_key(variable)
    }

    pub fn get(&self, variable: &String) -> Option<&usize> {
        self.variables.get(variable)
    }

    pub fn allocate_next(&mut self, variable: &String) -> usize {
        for i in 0..self.allocated_memory.len() {
            if !self.allocated_memory[i] {
                self.allocated_memory[i] = true;
                self.variables.insert(variable.clone(), i);
                return i;
            }
        }
        println!("DEBUG: Ram is full!\n");
        return 0;
    }

    pub fn free(&mut self, variable: &String) -> bool {
        if let Some(memory_location) = self.variables.remove(variable) {
            self.allocated_memory[memory_location] = false;
        }
        return false;
    }
}