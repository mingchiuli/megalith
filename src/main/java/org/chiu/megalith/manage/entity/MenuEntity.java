package org.chiu.megalith.manage.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

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
@NoArgsConstructor
@Builder
@DynamicUpdate
@Table(name ="m_menu")
public class MenuEntity {

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
        if (o == null || getClass() != o.getClass()) return false;

        MenuEntity that = (MenuEntity) o;

        if (!Objects.equals(menuId, that.menuId)) return false;
        if (!Objects.equals(parentId, that.parentId)) return false;
        if (!Objects.equals(title, that.title)) return false;
        if (!Objects.equals(name, that.name)) return false;
        if (!Objects.equals(url, that.url)) return false;
        if (!Objects.equals(component, that.component)) return false;
        if (!Objects.equals(type, that.type)) return false;
        if (!Objects.equals(icon, that.icon)) return false;
        if (!Objects.equals(orderNum, that.orderNum)) return false;
        return Objects.equals(status, that.status);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
