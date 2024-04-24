package org.chiu.megalith.blog.bloom;

public abstract class BloomHandler {

    public boolean supports(Class<? extends BloomHandler> clazz) {
        return clazz.equals(this.getClass());
    }

    public abstract void handle(Object[] args);
}
