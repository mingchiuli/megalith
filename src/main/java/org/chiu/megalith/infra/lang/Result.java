package org.chiu.megalith.infra.lang;

import java.util.function.Supplier;

import lombok.Data;

/**
 * @author mingchiuli
 * @create 2021-10-27 3:27 PM
 */
@Data
public class Result<T> {

    private String msg;

    private T data;

    public static <T> Result<T> success(T data) {
        return load("success",data); 
    }

    public static <T> Result<T> success() {
        return load("success",null); 
    }

    private static <T> Result<T> load(String msg, T data) {
        Result<T> r = new Result<>();
        r.setData(data);
        r.setMsg(msg);
        return r;
    }
    public static <T> Result<T> fail(String msg, T data) {
        return load(msg, data);
    }

    public static <T> Result<T> fail() {
        return load(null, null);
    }

    public static <T> Result<T> fail(String msg) {
        return load(msg, null);
    }

    public static Result<Void> success(Runnable runnable) {
        runnable.run();
        return success();
    }

    public static <T> Result<T> success(Supplier<T> supplier) {
        return Result.success(supplier.get());
    }

    public static <T> Result<T> fail(String msg, Runnable runnable) {
        runnable.run();
        return fail(msg);
    }
}
