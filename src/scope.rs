use crate::prelude::*;

use std::collections::HashMap;

pub static ROOT_PATH: &str = "#@";

pub enum ScopeNode {
    Scope(Box<Scope>),
    Variable(Box<LVariable>),
}

pub struct Scope {
    map: HashMap<String, ScopeNode>,
    path: String,
}

pub struct ScopeRoot {
    root: Scope,
}

impl ScopeRoot {
    pub fn insert_variable_at(&mut self, path: &str, name: &str, variable: LVariable) -> String {
        let scope = self.get_scope(path).unwrap();
        scope.insert_variable(name, variable);
        format!("{path}#{name}")
    }
    pub fn create_scope_at(&mut self, path: &str, name: &str) -> String {
        let scope = self.get_scope(path).unwrap();
        scope.create_scope(name)
    }
    pub fn create_scope_for_variable_at(&mut self, path: &str, name: &str) -> String {
        self.create_scope_at(path, &format!("{name}$"))
    }
    pub fn get_variable(&mut self, path: &str) -> Option<LVariable> {
        if path == ROOT_PATH {
            return None
        }
        // Remove root (#@)
        let path = path.strip_prefix(ROOT_PATH).unwrap_or(path);

        if let ScopeNode::Variable(var) = self.root.get_from_path(path)? {
            Some(*var.clone())
        } else {
            None
        }
    }
    pub fn get_variable_at(&mut self, path: &str, name: &str) -> Option<LVariable> {
        self.get_variable(&format!("{path}#{name}"))
    }
    fn get_scope(&mut self, path: &str) -> Option<&mut Scope> {
        if path == ROOT_PATH {
            return Some(&mut self.root)
        }
        // Remove root (#@)
        let path = path.strip_prefix(ROOT_PATH).unwrap_or(path);

        if let ScopeNode::Scope(scope) = self.root.get_from_path(path)? {
            Some(scope)
        } else {
            None
        }
    }
    pub fn find_variable_at(&mut self, path: &str, name: &str) -> Option<LVariable> {
        if path == ROOT_PATH {
            return None
        }
        // Remove root (#@)
        let mut path = path.strip_prefix(ROOT_PATH).unwrap_or(path);

        loop {
            let var = self.get_variable_at(path, name);
            if var.is_some() {
                return var;
            }

            let last_name_idx = path.rfind('#')?;
            path = &path[..last_name_idx];
        }
    }
}
impl Default for ScopeRoot {
    fn default() -> Self {
        Self {
            root: Scope {
                map: HashMap::new(),
                path: ROOT_PATH.to_string(),
            },
        }
    }
}

#[allow(dead_code)]
impl Scope {
    fn get_name(&self) -> String {
        let idx = self
            .path
            .rfind('#')
            .unwrap_or_else(|| panic!("Failed to parse path of Scope '{}'!", self.path));
        let name = &self.path[idx..];
        name.to_string()
    }

    fn insert_variable(&mut self, name: &str, variable: LVariable) {
        if self.map.contains_key(name) {
            panic!("Key '{name}' allready exists in scope '{}'", self.path);
        }

        self.map
            .insert(name.to_string(), ScopeNode::Variable(Box::new(variable)));
    }
    fn create_scope(&mut self, name: &str) -> String {
        if self.map.contains_key(name) {
            panic!("Key '{name}' allready exists in scope '{}'", self.path);
        }

        let path = format!("{}#{}", self.path, name);
        let new_scope = Scope {
            map: HashMap::new(),
            path: path.clone(),
        };

        self.map
            .insert(name.to_string(), ScopeNode::Scope(Box::new(new_scope)));

        path
    }
    fn create_scope_for_variable(&mut self, var_name: &str) -> String {
        let path = format!("{var_name}$");
        self.create_scope(&path)
    }
    fn get_value_from_scope(&self, name: &str) -> Option<LVariable> {
        let variable = self.map.get(name)?;

        match variable {
            ScopeNode::Scope(_) => None,
            ScopeNode::Variable(var) => Some(*var.clone()),
        }
    }

    fn get_from_path(&mut self, path: &str) -> Option<&mut ScopeNode> {
        // #main#foo#baa
        // #foo#baa
        // #baa

        // Remove #
        let path = path.strip_prefix('#').unwrap();

        if let Some(idx) = path.find('#') {
            let name = &path[..idx];

            let sub_scope = self.map.get_mut(name)?;

            if let ScopeNode::Scope(ref mut scope) = sub_scope {
                scope.get_from_path(&path[idx..])
            } else {
                None
            }
        } else {
            self.map.get_mut(path)
        }
    }

    fn find_value_in_scopes(&self, _scope_path: &str, _name: &str) -> Option<LVariable> {
        todo!()
    }
}
