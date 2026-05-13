---
title: Über das Projekt
slug: about
order: 1
published: true
description: Warum dieses Projekt existiert und wie Build-Time-Content im Elm-Projekt landet.
lang: de
updatedAt: 2026-05-13T00:00:00.000Z
---

Elm Minesweeper ist ein Spielprojekt, das neben dem eigentlichen Game auch Platz für statische Inhalte bekommen soll.

## Was diese Seite zeigt

- Frontmatter mit validierten Metadaten
- Build-Time generierte Elm-Module
- Routing innerhalb derselben Elm-SPA
- Graceful Fallback, falls Rendering einmal fehlschlägt

## Warum Build-Time?

Der Browser muss keine Markdown-Dateien entdecken oder parsen. Stattdessen werden die Inhalte vor dem Build in Elm-Code übersetzt.
