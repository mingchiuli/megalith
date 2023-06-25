package com.chiu.megalith.manage.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.Hibernate;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

import java.io.Serial;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:39 am
 */
@Entity
@Getter
@Setter
@ToString
@AllArgsConstructor
@Builder
@RequiredArgsConstructor
@DynamicUpdate
@Table(name ="m_menu")
public class MenuEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 18494689233749274L;

    @Id
    @Column(name = "menu_id")
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "native")
    @GenericGenerator(name = "native")
    private Long menuId;

    @Column(name = "parent_id")
    private Long parentId;

    @Column(name = "title")
    private String title;

    @Column(name = "name")
    private String name;

    @Column(name = "url")
    private String url;

    @Column(name = "component")
    private String component;

    @Column(name = "type")
    private Integer type;

    @Column(name = "icon")
    private String icon;

    @Column(name = "order_num")
    private Integer orderNum;

    @Column(name = "status")
    private Integer status;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        MenuEntity that = (MenuEntity) o;
        return menuId != null && Objects.equals(menuId, that.menuId);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
