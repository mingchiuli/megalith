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
                if (arg instanceof List) {
                    classes[i] = List.class;
                    continue;
                }
                if (arg instanceof Map) {
                    classes[i] = Map.class;
                    continue;
                }
                if (arg instanceof Set) {
                    classes[i] = Set.class;
                    continue;
                }
                classes[i] = arg.getClass();
            }
        }
        return classes;
    }
}
