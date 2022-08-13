---
title: "Enum starting with number"
slug: "enum-starting-with-number"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Java does not allow the name of enum to start with number like 100A, 25K. In that case, we can append the code with _ (underscore) or any allowed pattern and make check of it.

## Enum with name at begining
    public enum BookCode {
        _10A("Simon Haykin", "Communication System"),
        _42B("Stefan Hakins", "A Brief History of Time"),
        E1("Sedra Smith", "Electronics Circuits");

        private String author;
        private String title;

        BookCode(String author, String title) {
            this.author = author;
            this.title = title;
        }

        public String getName() {
            String name = name();
            if (name.charAt(0) == '_') {
                name = name.substring(1, name.length());
            }
            return name;
        }

        public static BookCode of(String code) {
            if (Character.isDigit(code.charAt(0))) {
                code = "_" + code;
            }
            return BookCode.valueOf(code);
        }
    }

