package com.chiu.megalith.ws.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class Container<B> implements Serializable {
    B data;
}
