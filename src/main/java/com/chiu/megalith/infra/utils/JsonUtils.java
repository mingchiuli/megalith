package com.chiu.megalith.infra.utils;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.stereotype.Component;



@Component
@RequiredArgsConstructor
public class JsonUtils {

    private final ObjectMapper objectMapper;

    @SneakyThrows
    public <T> T readValue(String str, Class<T> clazz) {
        return objectMapper.readValue(str, clazz);
    }

    @SneakyThrows
    public String writeValueAsString(Object obj) {
        return objectMapper.writeValueAsString(obj);
    }

}
