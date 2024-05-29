package org.chiu.megalith.infra.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ExceptionMessage {

    NO_AUTH(0, "没有权限，请重新登录"),

    NO_FOUND(1, "没有找到内容"),

    UPLOAD_MISS(3, "上传出现错误"),

    USER_MISS(4, "用户没有找到"),

    EDIT_NO_AUTH(5, "必须编辑自己的文章"),

    DELETE_NO_AUTH(6, "必须删除自己的文章"),

    ACCESSOR_NULL(7, "accessor is null"),

    TOKEN_INVALID(8, "token非法"),

    GET_LOCK_TIMEOUT(9, "获取锁超时"),

    AUTH_EXCEPTION(10, "认证异常"),

    ROLE_EXCEPTION(11, "角色异常"),

    MENU_NOT_EXIST(12, "menu不存在"),

    ROLE_NOT_EXIST(13, "role不存在"),

    USER_NOT_EXIST(14, "user不存在"),

    PASSWORD_REQUIRED(15, "需要密码"),

    EMAIL_NOT_EXIST(16, "email不存在"),

    WEB_NOT_EXIST(17, "web不存在"),

    DOCUMENT_NOT_EXIST(18, "web不存在"),

    CODE_TRY_MAX(19, "code reach max try number"),

    CODE_EXPIRED(20, "code expired"),

    CODE_MISMATCH(21, "code mismatch"),

    CODE_NOT_EXIST(22, "code not exist"),

    CODE_EXISTED(23, "code existed"),

    PASSWORD_MISMATCH(24, "Failed to authenticate since password does not match stored value"),

    PASSWORD_MISS(25, "Failed to authenticate since no credentials provided"),

    SMS_TRY_MAX(26, "sms reach max try number"),

    SMS_EXPIRED(27, "sms expired"),

    SMS_MISMATCH(28, "sms mismatch"),

    SMS_NOT_EXIST(29, "sms not exist"),

    PHONE_NOT_EXIST(30, "phone not exist"),

    ROLE_DISABLED(31, "role disabled"),

    MENU_INVALID_OPERATE(32, "先删除子菜单，不允许直接删除父菜单"),

    INVALID_LOGIN_OPERATE(33, "非法登录"),

    ACCOUNT_LOCKED(34, "账户被锁"),

    RE_LOGIN(35, "重新登里"),

    PASSWORD_DIFF(36, "账号密码不一致"),

    BUTTON_MUST_NOT_PARENT(37, "按钮不能有子元素"),

    MENU_CHILDREN_MUST_BE_BUTTON(38, "菜单子元素只能为按钮"),

    CATALOGUE_CHILD_MUST_NOT_BUTTON(39, "分类的子元素不能是按钮"),

    CATALOGUE_PARENT_MUST_PARENT(40, "分类的父元素只能是分类");

    private final Integer code;

    private final String msg;

}
