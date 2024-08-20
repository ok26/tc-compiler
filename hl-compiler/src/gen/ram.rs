use std::collections::HashMap;

pub struct Ram {
    variables: HashMap<String, usize>,
    allocated_memory: Vec<bool>,
    size: usize
}

impl Ram {
    pub fn new(size: usize) -> Ram {
        let mut allocated_memory = vec![false; size];
        allocated_memory[0] = true;
        Ram {
            variables: HashMap::new(),
            allocated_memory,
            size
        }
    }

    pub fn get(&self, variable: &String) -> Option<&usize> {
        self.variables.get(variable)
    }

    pub fn get_local_variables(&self, prefix: &String) -> Vec<String> {
        let mut local_variables = vec![];
        for variable in self.variables.keys() {
            if variable.starts_with(prefix) || variable.chars().nth(0).expect("Unreachable").is_numeric() {
                local_variables.push(variable.clone());
            }
        }
        local_variables
    }

    pub fn allocate_next(&mut self, variable: &String) -> usize {
        if let Some(location) = self.get(variable) { return *location; }

        let start_idx = if variable.chars().nth(0).expect("Unreachable").is_numeric() { self.size / 4 } else { 0 };
        for i in start_idx..self.allocated_memory.len() {
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
            return true;
        }
        return false;
    }
}