package com.chiu.megalith.common.generator;

import java.util.concurrent.ThreadLocalRandom;

/**
 * @author mingchiuli
 * @create 2023-03-05 1:04 am
 */
public class CodeGenerator {
    private static final char[] cs = {
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
            '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
    };

    public static String createText() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < 5; i++) {
            int idx = ThreadLocalRandom.current().nextInt(cs.length);
            builder.append(cs[idx]);
        }
        return builder.toString();
    }
}
