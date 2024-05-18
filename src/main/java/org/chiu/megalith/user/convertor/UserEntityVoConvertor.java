package org.chiu.megalith.user.convertor;

import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.user.vo.UserEntityVo;
import org.springframework.data.domain.Page;

import java.util.ArrayList;
import java.util.List;

public class UserEntityVoConvertor {

    private UserEntityVoConvertor() {}

    public static UserEntityVo convert(UserEntity userEntity) {
        return UserEntityVo.builder()
                .id(userEntity.getId())
                .username(userEntity.getUsername())
                .nickname(userEntity.getNickname())
                .avatar(userEntity.getAvatar())
                .email(userEntity.getEmail())
                .phone(userEntity.getPhone())
                .status(userEntity.getStatus())
                .created(userEntity.getCreated())
                .lastLogin(userEntity.getLastLogin())
                .role(userEntity.getRole())
                .build();
    }

    public static PageAdapter<UserEntityVo> convert(Page<UserEntity> page) {
        List<UserEntityVo> content = new ArrayList<>();
        page.getContent().forEach(user -> content
                .add(UserEntityVo.builder()
                        .email(user.getEmail())
                        .phone(user.getPhone())
                        .updated(user.getUpdated())
                        .role(user.getRole())
                        .id(user.getId())
                        .nickname(user.getNickname())
                        .status(user.getStatus())
                        .avatar(user.getAvatar())
                        .created(user.getCreated())
                        .lastLogin(user.getLastLogin())
                        .username(user.getUsername())
                        .build()));

        return PageAdapter.<UserEntityVo>builder()
                .empty(page.isEmpty())
                .first(page.isFirst())
                .last(page.isLast())
                .pageNumber(page.getPageable().getPageNumber())
                .content(content)
                .totalPages(page.getTotalPages())
                .pageSize(page.getSize())
                .totalElements(page.getTotalElements())
                .build();
    }
}
