package org.chiu.megalith.infra.utils;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class ClassUtils {

    public static Class<?>[] findClassArray(Object[] args) {
        var classes = new Class[args.length];
        for (int i = 0; i < args.length; i++) {
            Object arg = args[i];
            if (Objects.nonNull(arg)) {
                switch (arg) {
                    case List<?> ignored -> classes[i] = List.class;
                    case Map<?, ?> ignored -> classes[i] = Map.class;
                    case Set<?> ignored -> classes[i] = Set.class;
                    default -> classes[i] = arg.getClass();
                }

            }
        }
        return classes;
    }
}
