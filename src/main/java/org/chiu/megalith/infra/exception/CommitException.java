package org.chiu.megalith.infra.exception;

/**
 * @author mingchiuli
 * @create 2022-12-22 10:04 am
 */
public class CommitException extends RuntimeException {

    public CommitException(String message) {
        super(message);
    }
}
