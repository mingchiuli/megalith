package org.chiu.megalith.infra.utils;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;


@Component
@RequiredArgsConstructor
public class JsonUtils {

    private final ObjectMapper objectMapper;

    @SneakyThrows
    public <T> T readValue(String str, Class<T> clazz) {
        return objectMapper.readValue(str, clazz);
    }

    @SneakyThrows
    public <T> T convertValue(Object obj, Class<T> clazz) {
        return objectMapper.convertValue(obj, clazz);
    }

    @SneakyThrows
    public String writeValueAsString(Object obj) {
        return objectMapper.writeValueAsString(obj);
    }


    public JavaType getTypesReference(Method method) {

        Type genericReturnType = method.getGenericReturnType();
        JavaType javaType;

        if (genericReturnType instanceof ParameterizedType parameterizedType) {
            javaType = getTypesReference(parameterizedType);
        } else {
            javaType = objectMapper.getTypeFactory().constructType(genericReturnType);
        }

        return javaType;
    }

    private JavaType getTypesReference(ParameterizedType parameterizedType) {
        Class<?> rawType = (Class<?>) parameterizedType.getRawType();
        Type[] arguments = parameterizedType.getActualTypeArguments();
        var javaTypes = new JavaType[arguments.length];
        for (int i = 0; i < javaTypes.length; i++) {
            if (arguments[i] instanceof ParameterizedType parameterizedTyp) {
                JavaType type = getTypesReference(parameterizedTyp);
                javaTypes[i] = type;
            } else {
                javaTypes[i] = objectMapper.getTypeFactory().constructType(arguments[i]);
            }
        }
        return objectMapper.getTypeFactory().constructParametricType(rawType, javaTypes);
    }

}
