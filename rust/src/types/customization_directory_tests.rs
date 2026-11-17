// Copyright (c) Microsoft Corporation. All rights reserved.

#![cfg(test)]

use std::path::PathBuf;

use serde_json::Value;

use super::{CreateSessionOptions, ResumeSessionOptions, SessionId};

fn assert_roots(wire: &Value, roots: Option<&[PathBuf]>) {
    for kind in ["skills", "agents", "instructions", "hooks"] {
        match roots {
            Some(paths) => assert_eq!(
                wire[kind]["directories"],
                serde_json::to_value(paths.iter().map(|path| path.join(kind)).collect::<Vec<_>>())
                    .unwrap()
            ),
            None => assert!(
                wire[kind].get("directories").is_none(),
                "unexpected {kind}: {wire}"
            ),
        }
    }
    assert!(wire["filesystem"].get("additionalDirectories").is_none());
    assert!(wire["agents"].get("definitions").is_none());
    assert_eq!(wire["hooks"]["hasHandlers"], false);
    assert_eq!(wire["hooks"]["enableRepositoryHooks"], false);
    assert_eq!(wire["enableConfigDiscovery"], false);
}

#[test]
fn create_and_resume_preserve_discovery_roots_without_client_rebasing() {
    for roots in [
        None,
        Some(Vec::new()),
        Some(vec![
            PathBuf::from("relative-root"),
            PathBuf::from("other-root"),
        ]),
    ] {
        let mut create = CreateSessionOptions::default()
            .with_working_directory("request-cwd")
            .with_enable_config_discovery(false)
            .with_hooks(|group| group.with_enable_repository_hooks(false));
        let mut resume = ResumeSessionOptions::new(SessionId::from("resumed"))
            .with_working_directory("resume-cwd")
            .with_enable_config_discovery(false)
            .with_hooks(|group| group.with_enable_repository_hooks(false));
        if let Some(paths) = &roots {
            create = create
                .with_skills(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("skills")))
                })
                .with_agents(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("agents")))
                })
                .with_instructions(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("instructions")))
                })
                .with_hooks(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("hooks")))
                });
            resume = resume
                .with_skills(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("skills")))
                })
                .with_agents(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("agents")))
                })
                .with_instructions(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("instructions")))
                })
                .with_hooks(|group| {
                    group.with_directories(paths.iter().map(|path| path.join("hooks")))
                });
        }
        let (wire, _) = create.into_wire(None).unwrap();
        assert_roots(&serde_json::to_value(wire).unwrap(), roots.as_deref());
        let (wire, _) = resume.into_wire().unwrap();
        assert_roots(&serde_json::to_value(wire).unwrap(), roots.as_deref());
    }
}
