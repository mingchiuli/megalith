package com.chiu.megalith.authentication.user.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.Hibernate;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:18 am
 */
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
@Entity
@DynamicUpdate
@Table(name ="m_user", indexes = {@Index(columnList = "username")}, uniqueConstraints = {@UniqueConstraint(columnNames = {"username"})})
public class UserEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 18428328402842084L;

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "native")
    @GenericGenerator(name = "native", strategy = "native")
    private Long id;

    @Column(name = "username")
    private String username;

    @Column(name = "avatar")
    private String avatar;

    @Column(name = "email")
    private String email;

    @Column(name = "password")
    private String password;

    @Column(name = "status")
    private Integer status;

    @Column(name = "created")
    private LocalDateTime created;

    @Column(name = "last_login")
    private LocalDateTime lastLogin;

    @Column(name = "role")
    private String role;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        UserEntity that = (UserEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    public UserEntity(Long id, String username, String avatar, String email, String role) {
        this.id = id;
        this.username = username;
        this.avatar = avatar;
        this.email = email;
        this.role = role;
    }

    public UserEntity(String username) {
        this.username = username;
    }
}
