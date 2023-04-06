use std::collections::HashMap;

use crate::lvalue::LVariable;

pub enum ScopeNode {
    Scope(Box<Scope>),
    Variable(Box<LVariable>),
}

pub struct Scope {
    map: HashMap<String, ScopeNode>,
    path: String,
}

impl Scope {
    pub fn create_root() -> Self {
        Scope {
            map: HashMap::new(),
            path: "#@".to_string(),
        }
    }

    pub fn insert_variable(&mut self, name: &str, variable: LVariable) {
        if self.map.contains_key(name) {
            panic!("Key '{name}' allready exists in scope '{}'", self.path);
        }

        self.map
            .insert(name.to_string(), ScopeNode::Variable(Box::new(variable)));
    }
    pub fn create_scope(&mut self, name: &str) {
        if self.map.contains_key(name) {
            panic!("Key '{name}' allready exists in scope '{}'", self.path);
        }

        let new_scope = Scope {
            map: HashMap::new(),
            path: format!("{}#{}", self.path, name),
        };

        self.map
            .insert(name.to_string(), ScopeNode::Scope(Box::new(new_scope)));
    }
    pub fn get_name(&self) -> String {
        let idx = self
            .path
            .rfind('#')
            .unwrap_or_else(|| panic!("Failed to parse path of Scope '{}'!", self.path));
        let name = &self.path[idx..];
        name.to_string()
    }
    pub fn get_value_from_scope(&self, name: &str) -> Option<LVariable> {
        let variable = self.map.get(name)?;

        match variable {
            ScopeNode::Scope(_) => None,
            ScopeNode::Variable(var) => Some(*var.clone()),
        }
    }
}
